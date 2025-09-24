// SPDX-License-Identifier: MIT
//! GraphBuilder: collects edges and emits class-agnostic CSR.

use std::collections::HashSet;
use std::sync::Arc;

use super::{CaugiGraph, RegistrySnapshot};
use crate::edges::{EdgeRegistry, EdgeSpec, Mark};

#[derive(Debug)]
pub struct GraphBuilder {
    n: u32,
    simple: bool,
    specs: Arc<[EdgeSpec]>,
    rows: Vec<Vec<HalfEdge>>,
    seen: HashSet<(u32, u32, u8, bool)>,
    pair_seen: HashSet<(u32, u32)>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Side {
    Tail,
    Head,
}

impl From<Mark> for Side {
    fn from(m: Mark) -> Self {
        match m {
            Mark::Arrow => Side::Head,
            Mark::Tail | Mark::Circle | Mark::Other => Side::Tail,
        }
    }
}

impl Side {
    #[inline]
    fn as_u8(self) -> u8 {
        match self {
            Side::Tail => 0,
            Side::Head => 1,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct HalfEdge {
    nbr: u32,
    etype: u8,
    side: Side,
}

impl Ord for HalfEdge {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.nbr, self.etype, self.side.as_u8())
            .cmp(&(other.nbr, other.etype, other.side.as_u8()))
    }
}
impl PartialOrd for HalfEdge {
    fn partial_cmp(&self, o: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(o))
    }
}

impl GraphBuilder {
    pub fn new(n: u32, simple: bool, reg: &EdgeRegistry) -> Self {
        Self::new_with_registry(n, simple, reg)
    }

    pub fn new_with_registry(n: u32, simple: bool, reg: &EdgeRegistry) -> Self {
        let specs: Arc<[EdgeSpec]> = (0..reg.len() as u8)
            .map(|c| reg.spec_of_code(c).unwrap().clone())
            .collect::<Vec<_>>()
            .into();
        let n_us = n as usize;
        Self {
            n,
            simple,
            specs,
            rows: vec![Vec::new(); n_us],
            seen: HashSet::new(),
            pair_seen: HashSet::new(),
        }
    }

    pub fn add_edge(&mut self, u: u32, v: u32, etype: u8) -> Result<(), String> {
        if u >= self.n || v >= self.n {
            return Err("node id out of range".into());
        }
        if self.simple && u == v {
            return Err("self-loops not allowed in simple graphs".into());
        }

        let spec: EdgeSpec = self
            .specs
            .get(etype as usize)
            .cloned()
            .ok_or("invalid edge code")?;

        if self.simple {
            let (a, b) = if u <= v { (u, v) } else { (v, u) };
            if !self.pair_seen.insert((a, b)) {
                return Err("parallel edges not allowed in simple graphs".into());
            }
        }

        let key = if spec.symmetric {
            let (a, b) = if u <= v { (u, v) } else { (v, u) };
            (a, b, etype, true)
        } else {
            (u, v, etype, false)
        };
        if !self.seen.insert(key) {
            return Err("duplicate edge".into());
        }

        // Push halves based on marks.
        self.push_half(u, v, etype, spec.tail); // perspective u
        self.push_half(v, u, etype, spec.head); // perspective v
        Ok(())
    }

    fn push_half(&mut self, from: u32, to: u32, etype: u8, mark_at_from: Mark) {
        self.rows[from as usize].push(HalfEdge {
            nbr: to,
            etype,
            side: Side::from(mark_at_from),
        });
    }

    pub fn finalize(mut self) -> Result<CaugiGraph, String> {
        self.take_and_build()
    }

    pub fn finalize_in_place(&mut self) -> Result<CaugiGraph, String> {
        self.take_and_build()
    }

    fn take_and_build(&mut self) -> Result<CaugiGraph, String> {
        let rows = std::mem::take(&mut self.rows);
        let seen = std::mem::take(&mut self.seen);
        let _pair = std::mem::take(&mut self.pair_seen);
        self.build_from_rows(rows, seen)
    }

    fn build_from_rows(
        &mut self,
        mut rows: Vec<Vec<HalfEdge>>,
        _seen: HashSet<(u32, u32, u8, bool)>,
    ) -> Result<CaugiGraph, String> {
        let n = self.n as usize;

        for row in &mut rows {
            row.sort_unstable(); // uses Ord on HalfEdge
            if self.simple && row.windows(2).any(|w| w[0].nbr == w[1].nbr && w[0].etype == w[1].etype) {
                return Err(format!("parallel edge duplicate in row {:?}", row));
            }
        }

        let mut row_index = Vec::with_capacity(n + 1);
        row_index.push(0);
        for row in &rows { row_index.push(row_index.last().unwrap() + row.len() as u32); }

        let nnz = *row_index.last().unwrap() as usize;
        let mut col = vec![0u32; nnz];
        let mut ety  = vec![0u8;  nnz];
        let mut side = vec![0u8;  nnz];

        for (i, row) in rows.iter().enumerate() {
            let mut k = row_index[i] as usize;
            for h in row {
                col[k]  = h.nbr;
                ety[k]  = h.etype;
                side[k] = match h.side { Side::Tail => 0, Side::Head => 1 };
                k += 1;
            }
        }

        let snap = RegistrySnapshot::from_specs(self.specs.clone(), 1);
        CaugiGraph::from_csr(row_index, col, ety, side, snap)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;

    fn reg() -> EdgeRegistry {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        r
    }

    #[test]
    fn add_and_finalize_basic() {
        let r = reg();
        let cdir = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &r);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cdir).unwrap();
        let g = b.finalize().unwrap();
        assert_eq!(g.n(), 3);
        // row 0 has one out half-edge
        let rr0 = g.row_range(0);
        assert_eq!(rr0.len(), 1);
        assert_eq!(g.col_index[rr0.start], 1);
        // side=0 from tail at 0, side=1 from head at 1
        let rr1 = g.row_range(1);
        assert_eq!(g.side[rr0.start], 0);
        assert!(rr1.len() >= 1);
    }

        #[test]
    fn finalize_in_place_ok() {
        let r = reg();
        let cdir = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &r);
        b.add_edge(0, 1, cdir).unwrap();
        let g = b.finalize_in_place().unwrap();
        assert_eq!(g.n(), 2);
    }

    #[test]
    fn finalize_in_place_parallel_duplicate_row_error() {
        let r = reg();
        let cdir = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &r);
        // Manually craft duplicate entries
        b.rows[0].push(HalfEdge { nbr: 1, etype: cdir, side: Side::Tail });
        b.rows[0].push(HalfEdge { nbr: 1, etype: cdir, side: Side::Head }); // same nbr+etype, different side
        let err = b.finalize_in_place().unwrap_err();
        assert!(err.contains("parallel edge duplicate in row"));
    }

    #[test]
    fn simple_graph_blocks_parallel_and_self_loop() {
        let r = reg();
        let cdir = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &r);
        assert!(b.add_edge(0, 0, cdir).is_err()); // self-loop
        b.add_edge(0, 1, cdir).unwrap();
        assert!(b.add_edge(0, 1, cdir).is_err()); // parallel same type
    }

    #[test]
    fn duplicate_edge_rejected_even_if_asymmetric() {
        let r = reg();
        let cdir = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, false, &r);
        b.add_edge(0, 1, cdir).unwrap();
        assert!(b.add_edge(0, 1, cdir).is_err()); // exact duplicate
    }

    #[test]
    fn invalid_node_or_code() {
        let r = reg();
        let bad_code = 200;
        let mut b = GraphBuilder::new_with_registry(2, false, &r);
        assert!(b.add_edge(0, 2, bad_code).is_err()); // node out of range takes precedence
        // valid nodes but bad code
        assert!(b.add_edge(0, 1, bad_code).is_err());
    }
}
