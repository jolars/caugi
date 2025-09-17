// SPDX-License-Identifier: MIT
//! GraphBuilder: collects edges, enforces (optional) simplicity, and emits CSR.

use std::collections::HashSet;
use std::sync::Arc;

use crate::edges::{EdgeRegistry, EdgeSpec, Orientation, QueryFlags};
use super::{CaugiGraph, RegistrySnapshot};

#[derive(Debug)]
pub struct GraphBuilder {
    n: u32,
    simple: bool,
    /// Frozen view of the edge registry semantics (code -> spec).
    specs: Arc<[EdgeSpec]>,

    // Per-node buckets before sorting.
    parents:  Vec<Vec<HalfEdge>>,
    undirs:   Vec<Vec<HalfEdge>>,
    children: Vec<Vec<HalfEdge>>,

    /// Duplicate guard over *input* edges: (a,b,etype,is_symmetric_key).
    /// For symmetric glyphs, (a,b) is stored with a<=b; for asymmetric, the exact (u,v).
    seen: HashSet<(u32, u32, u8, bool)>,

    /// Parallel-edge guard when `simple`: (a,b).
    pair_seen: HashSet<(u32, u32)>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct HalfEdge {
    nbr: u32,
    etype: u8,
    side: u8, // 0=tail, 1=head
}

#[derive(Copy, Clone, Debug)]
enum Role { Parent, Undirected, Child }

impl GraphBuilder {
    /// Convenience: forwarder used by the R wrapper.
    pub fn new(n: u32, simple: bool, reg: &EdgeRegistry) -> Self {
        Self::new_with_registry(n, simple, reg)
    }

    /// Snapshot the registry; builder no longer depends on `EdgeRegistry` lifetime.
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
            parents: vec![Vec::new(); n_us],
            undirs: vec![Vec::new(); n_us],
            children: vec![Vec::new(); n_us],
            seen: HashSet::new(),
            pair_seen: HashSet::new(),
        }
    }

    /// Add a single logical edge (u,v,etype). Inserts both half-edges into row buckets.
    pub fn add_edge(&mut self, u: u32, v: u32, etype: u8) -> Result<(), String> {
        if u >= self.n || v >= self.n {
            return Err("node id out of range".into());
        }
        if self.simple && u == v {
            return Err("self-loops not allowed in simple graphs".into());
        }

        // Clone spec to avoid &self borrow across &mut self calls.
        let spec: EdgeSpec = self
            .specs
            .get(etype as usize)
            .cloned()
            .ok_or("invalid edge code")?;

        // Parallel-edge guard for simple graphs: forbid any second edge on the same pair.
        if self.simple {
            let (a, b) = if u <= v { (u, v) } else { (v, u) };
            if !self.pair_seen.insert((a, b)) {
                return Err("parallel edges not allowed in simple graphs".into());
            }
        }

        // Duplicate detection on *input* edges (same edge type).
        let key = if spec.symmetric {
            let (a, b) = if u <= v { (u, v) } else { (v, u) };
            (a, b, etype, true)
        } else {
            (u, v, etype, false)
        };
        if !self.seen.insert(key) {
            return Err("duplicate edge".into());
        }

        // Push both half-edges with correct side bit.
        match spec.orientation {
            Orientation::RightHead => {
                self.push_half(u, v, etype, 0, &spec); // u tail
                self.push_half(v, u, etype, 1, &spec); // v head
            }
            Orientation::LeftHead => {
                self.push_half(u, v, etype, 1, &spec); // u head
                self.push_half(v, u, etype, 0, &spec); // v tail
            }
            Orientation::BothHeads => {
                self.push_half(u, v, etype, 1, &spec);
                self.push_half(v, u, etype, 1, &spec);
            }
            Orientation::None => {
                self.push_half(u, v, etype, 0, &spec);
                self.push_half(v, u, etype, 0, &spec);
            }
        }
        Ok(())
    }

    fn push_half(&mut self, from: u32, to: u32, etype: u8, side_bit: u8, spec: &EdgeSpec) {
        use QueryFlags as F;

        let role = if side_bit == 0 {
            if spec.flags.contains(F::TAIL_PARENT) { Role::Parent }
            else if spec.flags.contains(F::TAIL_CHILD) { Role::Child }
            else { Role::Undirected }
        } else {
            if spec.flags.contains(F::HEAD_PARENT) { Role::Parent }
            else if spec.flags.contains(F::HEAD_CHILD) { Role::Child }
            else { Role::Undirected }
        };

        let bucket = match role {
            Role::Parent => &mut self.parents,
            Role::Undirected => &mut self.undirs,
            Role::Child => &mut self.children,
        };

        bucket[from as usize].push(HalfEdge { nbr: to, etype, side: side_bit });
    }

    /// Build a `CaugiGraph` and consume the builder.
    pub fn finalize(mut self) -> Result<CaugiGraph, String> {
        let parents = std::mem::take(&mut self.parents);
        let undirs = std::mem::take(&mut self.undirs);
        let children = std::mem::take(&mut self.children);
        let seen = std::mem::take(&mut self.seen);
        let _pair = std::mem::take(&mut self.pair_seen);
        self.build_from_parts(parents, undirs, children, seen)
    }

    /// Build a `CaugiGraph` and leave this builder re-usable (emptied).
    pub fn finalize_in_place(&mut self) -> Result<CaugiGraph, String> {
        let parents = std::mem::take(&mut self.parents);
        let undirs = std::mem::take(&mut self.undirs);
        let children = std::mem::take(&mut self.children);
        let seen = std::mem::take(&mut self.seen);
        let _pair = std::mem::take(&mut self.pair_seen);
        self.build_from_parts(parents, undirs, children, seen)
    }

    /// Core build routine shared by `finalize` and `finalize_in_place`.
    fn build_from_parts(
        &mut self,
        mut parents: Vec<Vec<HalfEdge>>,
        mut undirs: Vec<Vec<HalfEdge>>,
        mut children: Vec<Vec<HalfEdge>>,
        _seen: HashSet<(u32, u32, u8, bool)>,
    ) -> Result<CaugiGraph, String> {
        let n = self.n as usize;

        // Sort per row for determinism; check parallel dupes if simple.
        for i in 0..n {
            let by_key = |a: &HalfEdge, b: &HalfEdge| (a.nbr, a.etype, a.side).cmp(&(b.nbr, b.etype, b.side));
            parents[i].sort_unstable_by(by_key);
            undirs[i].sort_unstable_by(by_key);
            children[i].sort_unstable_by(by_key);

            if self.simple {
                let dup = |v: &[HalfEdge]| v.windows(2).any(|w| w[0].nbr == w[1].nbr && w[0].etype == w[1].etype);
                if dup(&parents[i]) || dup(&undirs[i]) || dup(&children[i]) {
                    return Err(format!("parallel edge duplicate in row {}", i));
                }
            }
        }

        // ROW_INDEX and SPLIT_INDEX.
        let mut row_index = Vec::with_capacity(n + 1);
        row_index.push(0u32);
        let mut split = Vec::with_capacity(n);
        let mut nnz: u32 = 0;
        for i in 0..n {
            let p = parents[i].len() as u32;
            let u = undirs[i].len() as u32;
            let c = children[i].len() as u32;
            nnz += p + u + c;
            split.push((p, u, c));
            row_index.push(nnz);
        }

        // Fill COL_INDEX, ETYPE, SIDE in order: parents | undirs | children.
        let mut col  = vec![0u32; nnz as usize];
        let mut ety  = vec![0u8; nnz as usize];
        let mut side = vec![0u8; nnz as usize];
        for i in 0..n {
            let mut k = row_index[i] as usize;
            for h in &parents[i] { col[k]=h.nbr; ety[k]=h.etype; side[k]=h.side; k+=1; }
            for h in &undirs[i] { col[k]=h.nbr; ety[k]=h.etype; side[k]=h.side; k+=1; }
            for h in &children[i] { col[k]=h.nbr; ety[k]=h.etype; side[k]=h.side; k+=1; }
            debug_assert_eq!(k, row_index[i + 1] as usize);
        }

        // Same registry snapshot that the builder was created with.
        let snap = RegistrySnapshot::from_specs(self.specs.clone(), 1);
        CaugiGraph::from_csr(row_index, col, ety, side, split, snap)
    }
}
