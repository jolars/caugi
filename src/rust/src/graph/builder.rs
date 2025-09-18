// SPDX-License-Identifier: MIT
//! GraphBuilder: collects edges, enforces (optional) simplicity,
//! builds base CSR [P|U|C] and three query views (PP, PC, UNK) using multi-role flags.

use std::collections::HashSet;
use std::sync::Arc;

use bitflags::bitflags;
use crate::edges::{EdgeRegistry, EdgeSpec, Orientation, QueryFlags};
use super::{CaugiGraph, RegistrySnapshot};

#[derive(Debug)]
pub struct GraphBuilder {
    n: u32,
    simple: bool,
    specs: Arc<[EdgeSpec]>,

    // Base per-node buckets: Parents | Undirected | Children | Unknowns
    parents: Vec<Vec<HalfEdge>>,
    undirected: Vec<Vec<HalfEdge>>,
    children: Vec<Vec<HalfEdge>>,
    unknowns: Vec<Vec<HalfEdge>>,

    // Guards
    seen: HashSet<(u32, u32, u8, bool)>,
    pair_seen: HashSet<(u32, u32)>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct HalfEdge { nbr: u32, etype: u8, side: u8 } // side: 0=tail, 1=head

bitflags! {
    struct Roles: u8 {
        const P = 1 << 0; // parent
        const U = 1 << 1; // undirected
        const C = 1 << 2; // child
        const PP = 1 << 3; // possible parent
        const PC = 1 << 4; // possible child
        const UNK = 1 << 5; // unknown
    }
}


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
            n, simple, specs,
            parents: vec![Vec::new(); n_us],
            undirected: vec![Vec::new(); n_us],
            children: vec![Vec::new(); n_us],
            unknowns: vec![Vec::new(); n_us],
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

    #[inline]
    fn roles_mask(side: u8, f: QueryFlags) -> Roles {
        use QueryFlags as F;
        let mut r = Roles::empty();
        if side == 0 {
            if f.contains(F::TAIL_CHILD) { r |= Roles::P; }
            if f.contains(F::TAIL_PARENT) { r |= Roles::C; }
            if f.contains(F::TAIL_UNDIR) { r |= Roles::U; }
            if f.contains(F::TAIL_UNKNOWN) { r |= Roles::UNK; }
            if f.contains(F::HEAD_POSS_PARENT) { r |= Roles::PP; }
            if f.contains(F::HEAD_POSS_CHILD) { r |= Roles::PC; }
        } else {
            if f.contains(F::HEAD_CHILD) { r |= Roles::P; }
            if f.contains(F::HEAD_PARENT) { r |= Roles::C; }
            if f.contains(F::HEAD_UNDIR) { r |= Roles::U; }
            if f.contains(F::HEAD_UNKNOWN) { r |= Roles::UNK; }
            if f.contains(F::TAIL_POSS_PARENT) { r |= Roles::PP; }
            if f.contains(F::TAIL_POSS_CHILD) { r |= Roles::PC; }
        }
        r
    }

    fn push_half(&mut self, from: u32, to: u32, etype: u8, side: u8, spec: &EdgeSpec) {
        let m = Self::roles_mask(side, spec.flags);

        // Base placement: P/C to their buckets; U to undirected.
        if m.contains(Roles::P) {
            self.parents[from as usize].push(HalfEdge{ nbr: to, etype, side });
        } else if m.contains(Roles::C) {
            self.children[from as usize].push(HalfEdge{ nbr: to, etype, side });
        } else if m.contains(Roles::U) {
            self.undirected[from as usize].push(HalfEdge{ nbr: to, etype, side });
        } else if m.contains(Roles::UNK) {
            self.unknowns[from as usize].push(HalfEdge{ nbr: to, etype, side });
        }

        // Poss-only edges with no P/C/U still contribute to views at finalize; no base bucket needed.
    }

    pub fn finalize(mut self) -> Result<CaugiGraph, String> {
        let parents = std::mem::take(&mut self.parents);
        let undirected = std::mem::take(&mut self.undirected);
        let children = std::mem::take(&mut self.children);
        let unknowns = std::mem::take(&mut self.unknowns);
        let seen = std::mem::take(&mut self.seen);
        let _pair = std::mem::take(&mut self.pair_seen);
        self.build_from_parts(parents, undirected, children, unknowns, seen)
    }

    /// Build a `CaugiGraph` and leave this builder re-usable (emptied).
    pub fn finalize_in_place(&mut self) -> Result<CaugiGraph, String> {
        let parents = std::mem::take(&mut self.parents);
        let undirected = std::mem::take(&mut self.undirected);
        let children = std::mem::take(&mut self.children);
        let unknowns = std::mem::take(&mut self.unknowns);
        let seen = std::mem::take(&mut self.seen);
        let _pair = std::mem::take(&mut self.pair_seen);
        self.build_from_parts(parents, undirected, children, unknowns, seen)
    }

    /// Core build routine shared by `finalize` and `finalize_in_place`.
    fn build_from_parts(
        &mut self,
        mut parents: Vec<Vec<HalfEdge>>,
        mut undirected: Vec<Vec<HalfEdge>>,
        mut children: Vec<Vec<HalfEdge>>,
        mut unknowns: Vec<Vec<HalfEdge>>,
        _seen: HashSet<(u32, u32, u8, bool)>,
    ) -> Result<CaugiGraph, String> {
        let n = self.n as usize;

        // Sort and dup-check base + unknowns
        for i in 0..n {
            let by_key = |a: &HalfEdge, b: &HalfEdge| (a.nbr, a.etype, a.side).cmp(&(b.nbr, b.etype, b.side));
            parents[i].sort_unstable_by(by_key);
            undirected[i].sort_unstable_by(by_key);
            children[i].sort_unstable_by(by_key);
            unknowns[i].sort_unstable_by(by_key);

            if self.simple {
                let dup = | v: &[HalfEdge] | v.windows(2).any(|w| w[0].nbr==w[1].nbr && w[0].etype==w[1].etype);
                if dup(&parents[i]) || dup(&undirected[i]) || dup(&children[i]) || dup(&unknowns[i]) {
                    return Err(format!("parallel edge duplicate in row {}", i));
                }
            }
        }

        // BASE CSR: [P | U | C]
        let mut row_index = Vec::with_capacity(n+1); row_index.push(0);
        let mut split = Vec::with_capacity(n); // (p,u,c)
        let mut nnz: u32 = 0;
        for i in 0..n {
            let p = parents[i].len() as u32;
            let u = undirected[i].len() as u32;
            let c = children[i].len() as u32;
            nnz += p + u + c;
            split.push((p, u, c));
            row_index.push(nnz);
        }

        let mut col  = vec![0u32; nnz as usize];
        let mut ety  = vec![0u8; nnz as usize];
        let mut side = vec![0u8; nnz as usize];
        for i in 0..n {
            let mut k = row_index[i] as usize;
            for h in &parents[i] { col[k]=h.nbr; ety[k]=h.etype; side[k]=h.side; k+=1; }
            for h in &undirected[i] { col[k]=h.nbr; ety[k]=h.etype; side[k]=h.side; k+=1; }
            for h in &children[i] { col[k]=h.nbr; ety[k]=h.etype; side[k]=h.side; k+=1; }
            debug_assert_eq!(k, row_index[i + 1] as usize);
        }

        // VIEWS: PP, PC, UNK
        let mut pp_row = Vec::with_capacity(n+1); pp_row.push(0);
        let mut pc_row = Vec::with_capacity(n+1); pc_row.push(0);
        let mut uk_row = Vec::with_capacity(n+1); uk_row.push(0);
        let mut pp_col: Vec<u32> = Vec::new();
        let mut pc_col: Vec<u32> = Vec::new();
        let mut uk_col: Vec<u32> = Vec::new();

        for i in 0..n {
            let mut tmp_pp: Vec<u32> = Vec::new();
            let mut tmp_pc: Vec<u32> = Vec::new();
            let mut tmp_uk: Vec<u32> = Vec::new();

            // scan all halves including unknowns
            for h in parents[i].iter()
                .chain(undirected[i].iter())
                .chain(children[i].iter())
                .chain(unknowns[i].iter()) {

                let m = Self::roles_mask(h.side, self.specs[h.etype as usize].flags);
                if m.intersects(Roles::P | Roles::PP) { tmp_pp.push(h.nbr); }
                if m.intersects(Roles::C | Roles::PC) { tmp_pc.push(h.nbr); }
                if m.contains(Roles::UNK)             { tmp_uk.push(h.nbr); }
            }

            tmp_pp.sort_unstable(); tmp_pp.dedup();
            tmp_pc.sort_unstable(); tmp_pc.dedup();
            tmp_uk.sort_unstable(); tmp_uk.dedup();

            pp_col.extend_from_slice(&tmp_pp); pp_row.push(pp_col.len() as u32);
            pc_col.extend_from_slice(&tmp_pc); pc_row.push(pc_col.len() as u32);
            uk_col.extend_from_slice(&tmp_uk); uk_row.push(uk_col.len() as u32);
        }

        let snap = RegistrySnapshot::from_specs(self.specs.clone(), 1);
        CaugiGraph::from_csr(
            row_index, col, ety, side, split,
            pp_row, pp_col,
            pc_row, pc_col,
            uk_row, uk_col,
            snap,
        )
    }
}
