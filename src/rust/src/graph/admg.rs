// SPDX-License-Identifier: MIT
//! ADMG (Acyclic Directed Mixed Graph) wrapper with O(1) slice queries via packed neighborhoods.
//!
//! An ADMG contains:
//! - Directed edges (-->) representing direct causal effects
//! - Bidirected edges (<->) representing latent confounding
//!
//! The directed part must be acyclic.

use super::CaugiGraph;
use crate::edges::EdgeClass;
use crate::graph::alg::bitset;
use crate::graph::alg::directed_part_is_acyclic;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Admg {
    core: Arc<CaugiGraph>,
    /// len = n+1; prefix sums for neighborhood slices
    node_edge_ranges: Arc<[usize]>,
    /// len = n; (parents, spouses, children) counts per node
    node_deg: Arc<[(u32, u32, u32)]>,
    /// packed as [parents | spouses | children] for each node
    neighborhoods: Arc<[u32]>,
}

impl Admg {
    /// Builds an `Admg` view over a class-agnostic CSR graph.
    ///
    /// Validates that:
    /// 1. The directed part is acyclic
    /// 2. Only directed and bidirected edges are present
    ///
    /// Parents, spouses, and children for each node are stored contiguously and sorted.
    pub fn new(core: Arc<CaugiGraph>) -> Result<Self, String> {
        let n = core.n() as usize;

        // Check acyclicity of directed part
        if !directed_part_is_acyclic(&core) {
            return Err("ADMG contains a directed cycle".into());
        }

        // Count (parents, spouses, children) per node
        let mut deg: Vec<(u32, u32, u32)> = vec![(0, 0, 0); n];
        for i in 0..n {
            for k in core.row_range(i as u32) {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Directed => {
                        if core.side[k] == 1 {
                            deg[i].0 += 1; // parent
                        } else {
                            deg[i].2 += 1; // child
                        }
                    }
                    EdgeClass::Bidirected => {
                        deg[i].1 += 1; // spouse
                    }
                    _ => {
                        return Err(
                            "ADMG can only contain directed and bidirected edges".into()
                        );
                    }
                }
            }
        }

        // Prefix sums for row slices into `neighborhoods`
        let mut node_edge_ranges = Vec::with_capacity(n + 1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let (pa, sp, ch) = deg[i];
            let last = *node_edge_ranges.last().unwrap();
            node_edge_ranges.push(last + (pa + sp + ch) as usize);
        }
        let total = *node_edge_ranges.last().unwrap();
        let mut neigh = vec![0u32; total];

        // Bucket bases for scatter
        let mut parent_base: Vec<usize> = vec![0; n];
        let mut spouse_base: Vec<usize> = vec![0; n];
        let mut child_base: Vec<usize> = vec![0; n];
        for i in 0..n {
            let start = node_edge_ranges[i];
            let (pa, sp, _) = deg[i];
            parent_base[i] = start;
            spouse_base[i] = start + pa as usize;
            child_base[i] = spouse_base[i] + sp as usize;
        }
        let mut pcur = parent_base.clone();
        let mut scur = spouse_base.clone();
        let mut ccur = child_base.clone();

        // Scatter pass
        for i in 0..n {
            for k in core.row_range(i as u32) {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Directed => {
                        if core.side[k] == 1 {
                            let p = pcur[i];
                            neigh[p] = core.col_index[k];
                            pcur[i] += 1;
                        } else {
                            let p = ccur[i];
                            neigh[p] = core.col_index[k];
                            ccur[i] += 1;
                        }
                    }
                    EdgeClass::Bidirected => {
                        let p = scur[i];
                        neigh[p] = core.col_index[k];
                        scur[i] += 1;
                    }
                    _ => unreachable!("Should have errored on invalid edges earlier"),
                }
            }
            // Sort each segment for determinism and binary search
            let s = node_edge_ranges[i];
            let pm = spouse_base[i];
            let sm = child_base[i];
            let e = node_edge_ranges[i + 1];
            neigh[s..pm].sort_unstable();
            neigh[pm..sm].sort_unstable();
            neigh[sm..e].sort_unstable();
        }

        Ok(Self {
            core,
            node_edge_ranges: node_edge_ranges.into(),
            node_deg: deg.into(),
            neighborhoods: neigh.into(),
        })
    }

    /// Number of nodes.
    #[inline]
    pub fn n(&self) -> u32 {
        self.core.n()
    }

    /// Returns (row_start, parents_end, spouses_end, children_start) for node `i`.
    #[inline]
    fn bounds(&self, i: u32) -> (usize, usize, usize, usize) {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        let (pa, sp, ch) = self.node_deg[i];
        let pm = s + pa as usize;
        let sm = pm + sp as usize;
        let cs = e - ch as usize;
        (s, pm, sm, cs)
    }

    /// Sorted slice of parents of `i` (nodes with directed edge into `i`).
    #[inline]
    pub fn parents_of(&self, i: u32) -> &[u32] {
        let (s, pm, _, _) = self.bounds(i);
        &self.neighborhoods[s..pm]
    }

    /// Sorted slice of children of `i` (nodes with directed edge from `i`).
    #[inline]
    pub fn children_of(&self, i: u32) -> &[u32] {
        let (_, _, _, cs) = self.bounds(i);
        let e = self.node_edge_ranges[i as usize + 1];
        &self.neighborhoods[cs..e]
    }

    /// Sorted slice of spouses of `i` (nodes connected via bidirected edge).
    #[inline]
    pub fn spouses_of(&self, i: u32) -> &[u32] {
        let (_, pm, sm, _) = self.bounds(i);
        &self.neighborhoods[pm..sm]
    }

    /// All neighbors of `i`: [parents | spouses | children].
    #[inline]
    pub fn neighbors_of(&self, i: u32) -> &[u32] {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        &self.neighborhoods[s..e]
    }

    /// All ancestors of `i` via directed edges, returned in ascending order.
    #[inline]
    pub fn ancestors_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut stack: Vec<u32> = self.parents_of(i).to_vec();
        while let Some(u) = stack.pop() {
            let ui = u as usize;
            if std::mem::replace(&mut seen[ui], true) {
                continue;
            }
            stack.extend_from_slice(self.parents_of(u));
        }
        bitset::collect_from_mask(&seen)
    }

    /// All descendants of `i` via directed edges, returned in ascending order.
    #[inline]
    pub fn descendants_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut stack: Vec<u32> = self.children_of(i).to_vec();
        while let Some(u) = stack.pop() {
            let ui = u as usize;
            if std::mem::replace(&mut seen[ui], true) {
                continue;
            }
            stack.extend_from_slice(self.children_of(u));
        }
        bitset::collect_from_mask(&seen)
    }

    /// Markov blanket of `i` in an ADMG:
    /// `Pa(i) ∪ Ch(i) ∪ Sp(i) ∪ (⋃ Pa(c) \ {i} : c∈Ch(i)) ∪ (⋃ Pa(s) : s∈Sp(i))`.
    #[inline]
    pub fn markov_blanket_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut m = vec![false; n];

        // Parents
        for &p in self.parents_of(i) {
            m[p as usize] = true;
        }

        // Children
        for &c in self.children_of(i) {
            m[c as usize] = true;
            // Co-parents of children
            for &p in self.parents_of(c) {
                if p != i {
                    m[p as usize] = true;
                }
            }
        }

        // Spouses (bidirected neighbors)
        for &s in self.spouses_of(i) {
            m[s as usize] = true;
            // Parents of spouses (for d-separation purposes in ADMGs)
            for &p in self.parents_of(s) {
                m[p as usize] = true;
            }
        }

        m[i as usize] = false; // exclude self
        bitset::collect_from_mask(&m)
    }

    /// Nodes with no parents (exogenous in directed sense).
    #[inline]
    pub fn exogenous_nodes(&self) -> Vec<u32> {
        (0..self.n())
            .filter(|&i| self.parents_of(i).is_empty())
            .collect()
    }

    /// Access the underlying CSR.
    pub fn core_ref(&self) -> &CaugiGraph {
        &self.core
    }
}

// ── M-Separation ──────────────────────────────────────────────────────────────
impl Admg {
    /// Ancestors mask including the seeds themselves.
    /// `mask[v] == true` iff `v ∈ An(seeds) ∪ seeds`.
    fn ancestors_mask(&self, seeds: &[u32]) -> Vec<bool> {
        bitset::ancestors_mask(seeds, |u| self.parents_of(u), self.n())
    }

    /// Build a moral adjacency for m-separation in an ADMG.
    ///
    /// This extends the standard moralization to handle bidirected edges:
    /// 1. Connect parents with children (undirected)
    /// 2. Marry parents of common children
    /// 3. Add bidirected edges as undirected edges
    fn moral_adj_admg(&self, mask: &[bool]) -> Vec<Vec<u32>> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];

        for v in 0..n as u32 {
            if !mask[v as usize] {
                continue;
            }

            // Connect with parents (as in standard moralization)
            let pa = self.parents_of(v);
            for &p in pa {
                if mask[p as usize] {
                    adj[v as usize].push(p);
                    adj[p as usize].push(v);
                }
            }

            // Marry parents of common children
            for i in 0..pa.len() {
                let pi = pa[i] as usize;
                if !mask[pi] {
                    continue;
                }
                for j in i + 1..pa.len() {
                    let pj = pa[j] as usize;
                    if !mask[pj] {
                        continue;
                    }
                    adj[pi].push(pa[j]);
                    adj[pj].push(pa[i]);
                }
            }

            // Add bidirected edges as undirected (spouses connect in moral graph)
            for &s in self.spouses_of(v) {
                if mask[s as usize] {
                    adj[v as usize].push(s);
                    // Note: the reverse will be added when we process node s
                }
            }
        }

        // Dedup neighbors per node
        for v in &mut adj {
            v.sort_unstable();
            v.dedup();
        }
        adj
    }

    /// BFS reachability check over moral graph.
    /// Returns true if any node in `src` can reach any node in `tgt` while avoiding `blocked`.
    fn reachable_in_moral(
        adj: &[Vec<u32>],
        mask: &[bool],
        src: &[u32],
        blocked: &[bool],
        tgt: &[u32],
    ) -> bool {
        use std::collections::VecDeque;
        let n = adj.len();
        let mut target = vec![false; n];
        for &y in tgt {
            if !blocked[y as usize] {
                target[y as usize] = true;
            }
        }

        let mut seen = vec![false; n];
        let mut q = VecDeque::new();
        for &x in src {
            let xi = x as usize;
            if mask[xi] && !blocked[xi] && !seen[xi] {
                seen[xi] = true;
                q.push_back(x);
            }
        }

        while let Some(u) = q.pop_front() {
            let ui = u as usize;
            if target[ui] {
                return true;
            }
            for &w in &adj[ui] {
                let wi = w as usize;
                if mask[wi] && !blocked[wi] && !seen[wi] {
                    seen[wi] = true;
                    q.push_back(w);
                }
            }
        }
        false
    }

    /// M-separation test for ADMGs.
    ///
    /// Tests whether `xs` is m-separated from `ys` given `z` in the ADMG.
    ///
    /// M-separation generalizes d-separation to ADMGs by:
    /// 1. Taking the ancestral subgraph of `xs ∪ ys ∪ z`
    /// 2. Moralizing it (marrying parents, keeping bidirected edges)
    /// 3. Removing conditioning nodes
    /// 4. Testing path connectivity
    ///
    /// Returns `true` iff `xs ⊥_m ys | z`.
    pub fn m_separated(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> bool {
        if xs.is_empty() || ys.is_empty() {
            return true;
        }

        // Collect all seeds
        let mut seeds = xs.to_vec();
        seeds.extend_from_slice(ys);
        seeds.extend_from_slice(z);
        seeds.sort_unstable();
        seeds.dedup();

        // Get ancestral mask
        let mask = self.ancestors_mask(&seeds);

        // Build moral adjacency
        let adj = self.moral_adj_admg(&mask);

        // Block conditioned nodes
        let mut blocked = vec![false; self.n() as usize];
        for &v in z {
            blocked[v as usize] = true;
        }

        // Check if xs can reach ys in the moral graph
        !Self::reachable_in_moral(&adj, &mask, xs, &blocked, ys)
    }
}

// ── Adjustment Criteria ───────────────────────────────────────────────────────
impl Admg {
    /// Descendants mask including seeds.
    fn descendants_mask(&self, seeds: &[u32]) -> Vec<bool> {
        bitset::descendants_mask(seeds, |u| self.children_of(u), self.n())
    }

    /// Check if node `b` is reachable from node `a` via directed paths.
    fn has_directed_path(&self, a: u32, b: u32) -> bool {
        if a == b {
            return true;
        }
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut stack = vec![a];

        while let Some(u) = stack.pop() {
            if u == b {
                return true;
            }
            if std::mem::replace(&mut seen[u as usize], true) {
                continue;
            }
            stack.extend_from_slice(self.children_of(u));
        }
        false
    }

    /// Compute the "forbidden" set for adjustment:
    /// Descendants of X that lie on a proper causal path from X to Y.
    fn forbidden_set(&self, xs: &[u32], ys: &[u32]) -> Vec<bool> {
        let n = self.n() as usize;
        let mut forbidden = vec![false; n];

        // Compute De(X)
        let de_x = self.descendants_mask(xs);

        // For each y ∈ Y, find all nodes on causal paths from X to y
        for &y in ys {
            // A node is on a causal path if it's a descendant of X and ancestor of Y
            // that can reach Y via directed path
            for v in 0..n as u32 {
                if de_x[v as usize] && self.has_directed_path(v, y) {
                    forbidden[v as usize] = true;
                }
            }
        }

        // Remove X itself from forbidden (we exclude X from adjustment anyway)
        for &x in xs {
            forbidden[x as usize] = false;
        }

        forbidden
    }

    /// Validate whether Z is a valid adjustment set for estimating X → Y.
    ///
    /// Uses the generalized adjustment criterion for ADMGs:
    /// 1. Z must not contain any descendant of X on a proper causal path to Y
    /// 2. Z must m-separate X from Y in the proper backdoor graph
    ///
    /// The proper backdoor graph is obtained by removing the first edge
    /// on every proper causal path from X to Y.
    pub fn is_valid_adjustment_set(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> bool {
        // Check condition 1: Z contains no forbidden nodes
        let forbidden = self.forbidden_set(xs, ys);
        for &v in z {
            if forbidden[v as usize] {
                return false;
            }
        }

        // Check condition 2: X and Y are m-separated given Z in the proper backdoor graph
        // For ADMGs, we need to check m-separation while considering that:
        // - Proper causal paths from X to Y should be "blocked"
        // - Bidirected edges still create confounding paths

        // Build the conditioning set: Z ∪ X (we condition on X to block causal paths)
        let mut obs = Vec::with_capacity(z.len() + xs.len());
        obs.extend_from_slice(z);
        obs.extend_from_slice(xs);

        // For each x ∈ X, check that all backdoor paths are blocked
        // A backdoor path starts with an edge INTO x or a bidirected edge from x
        for &x in xs {
            // Check parents of x (backdoor through directed edges into x)
            for &p in self.parents_of(x) {
                if !self.m_separated(&[p], ys, &obs) {
                    return false;
                }
            }

            // Check spouses of x (backdoor through bidirected edges)
            for &s in self.spouses_of(x) {
                if !self.m_separated(&[s], ys, &obs) {
                    return false;
                }
            }
        }

        true
    }

    /// Simple parent-based adjustment set for ADMGs.
    /// Returns Pa(X) \ (X ∪ Y) if it's a valid adjustment set.
    pub fn adjustment_set_parents(&self, xs: &[u32], ys: &[u32]) -> Option<Vec<u32>> {
        let n = self.n() as usize;
        let mut keep = vec![false; n];

        // Collect parents of X
        for &x in xs {
            for &p in self.parents_of(x) {
                keep[p as usize] = true;
            }
        }

        // Exclude X ∪ Y
        for &x in xs {
            keep[x as usize] = false;
        }
        for &y in ys {
            keep[y as usize] = false;
        }

        let z = bitset::collect_from_mask(&keep);

        // Validate
        if self.is_valid_adjustment_set(xs, ys, &z) {
            Some(z)
        } else {
            None
        }
    }

    /// Backdoor adjustment set for ADMGs.
    /// Returns (Pa(X) ∩ An(Y)) \ (De(X) ∪ X ∪ Y) if valid.
    pub fn adjustment_set_backdoor(&self, xs: &[u32], ys: &[u32]) -> Option<Vec<u32>> {
        let n = self.n() as usize;

        // An(Y)
        let an_mask = self.ancestors_mask(ys);

        // Pa(X) ∩ An(Y)
        let mut keep = vec![false; n];
        for &x in xs {
            for &p in self.parents_of(x) {
                if an_mask[p as usize] {
                    keep[p as usize] = true;
                }
            }
        }

        // Exclude De(X) ∪ X ∪ Y
        let de_mask = self.descendants_mask(xs);
        for i in 0..n {
            if de_mask[i] {
                keep[i] = false;
            }
        }
        for &x in xs {
            keep[x as usize] = false;
        }
        for &y in ys {
            keep[y as usize] = false;
        }

        let z = bitset::collect_from_mask(&keep);

        if self.is_valid_adjustment_set(xs, ys, &z) {
            Some(z)
        } else {
            None
        }
    }

    /// Enumerate all valid adjustment sets up to a given size.
    pub fn all_adjustment_sets(
        &self,
        xs: &[u32],
        ys: &[u32],
        minimal: bool,
        max_size: u32,
    ) -> Vec<Vec<u32>> {
        // Universe of candidates: not in X, Y, or De(X) on causal paths
        let de_mask = self.descendants_mask(xs);
        let universe: Vec<u32> = (0..self.n())
            .filter(|&v| {
                let vi = v as usize;
                !de_mask[vi]  // Not a descendant of X
                    && xs.iter().all(|&x| x != v)
                    && ys.iter().all(|&y| y != v)
            })
            .collect();

        let mut valid_sets = Vec::new();
        let mut cur = Vec::new();
        let max_k = (max_size as usize).min(universe.len());

        // Enumerate all subsets up to max_size
        for k in 0..=max_k {
            Self::k_subsets(&universe, k, 0, &mut cur, &mut valid_sets, |z| {
                self.is_valid_adjustment_set(xs, ys, z)
            });
        }

        if minimal {
            Self::prune_minimal(&mut valid_sets);
        }

        valid_sets
    }

    /// Enumerate all k-subsets of `u`, collecting those satisfying `pred`.
    fn k_subsets<F>(
        u: &[u32],
        k: usize,
        start: usize,
        cur: &mut Vec<u32>,
        out: &mut Vec<Vec<u32>>,
        pred: F,
    ) where
        F: Fn(&[u32]) -> bool + Copy,
    {
        if cur.len() == k {
            if pred(cur) {
                out.push(cur.clone());
            }
            return;
        }
        for i in start..u.len() {
            cur.push(u[i]);
            Self::k_subsets(u, k, i + 1, cur, out, pred);
            cur.pop();
        }
    }

    /// Remove non-minimal supersets, keeping only inclusion-minimal sets.
    fn prune_minimal(sets: &mut Vec<Vec<u32>>) {
        sets.iter_mut().for_each(|v| {
            v.sort_unstable();
            v.dedup();
        });
        sets.sort();

        let mut out: Vec<Vec<u32>> = Vec::new();
        'next: for z in sets.drain(..) {
            // Skip if z is a superset of some existing set
            for s in &out {
                if s.iter().all(|v| z.binary_search(v).is_ok()) {
                    continue 'next;
                }
            }
            // Remove any existing superset of z
            out.retain(|s| !z.iter().all(|v| s.binary_search(v).is_ok()));
            out.push(z);
        }
        *sets = out;
    }
}

// ── Districts (C-Components) ──────────────────────────────────────────────────
impl Admg {
    /// Compute all districts (c-components) of the ADMG.
    ///
    /// A district is a maximal set of nodes connected via bidirected edges.
    /// Returns a vector of districts, each district being a sorted vector of node indices.
    pub fn districts(&self) -> Vec<Vec<u32>> {
        let n = self.n() as usize;
        let mut comp = vec![usize::MAX; n];
        let mut stack = Vec::new();
        let mut cid = 0usize;

        for s in 0..n {
            if comp[s] != usize::MAX {
                continue;
            }
            // Start a new district
            comp[s] = cid;
            stack.clear();
            stack.push(s as u32);

            // BFS/DFS over bidirected edges
            while let Some(u) = stack.pop() {
                for &w in self.spouses_of(u) {
                    let wi = w as usize;
                    if comp[wi] == usize::MAX {
                        comp[wi] = cid;
                        stack.push(w);
                    }
                }
            }
            cid += 1;
        }

        // Collect nodes into districts
        let mut districts: Vec<Vec<u32>> = vec![Vec::new(); cid];
        for (node, &c) in comp.iter().enumerate() {
            districts[c].push(node as u32);
        }

        // Sort each district for determinism
        for d in &mut districts {
            d.sort_unstable();
        }

        districts
    }

    /// Get the district (c-component) containing node `i`.
    ///
    /// Returns a sorted vector of all nodes in the same district as `i`.
    pub fn district_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut stack = vec![i];
        seen[i as usize] = true;

        while let Some(u) = stack.pop() {
            for &w in self.spouses_of(u) {
                let wi = w as usize;
                if !seen[wi] {
                    seen[wi] = true;
                    stack.push(w);
                }
            }
        }

        bitset::collect_from_mask(&seen)
    }

    /// Get the district assignment for all nodes.
    ///
    /// Returns a vector where `result[i]` is the district index of node `i`.
    pub fn district_membership(&self) -> Vec<usize> {
        let n = self.n() as usize;
        let mut comp = vec![usize::MAX; n];
        let mut stack = Vec::new();
        let mut cid = 0usize;

        for s in 0..n {
            if comp[s] != usize::MAX {
                continue;
            }
            comp[s] = cid;
            stack.clear();
            stack.push(s as u32);

            while let Some(u) = stack.pop() {
                for &w in self.spouses_of(u) {
                    let wi = w as usize;
                    if comp[wi] == usize::MAX {
                        comp[wi] = cid;
                        stack.push(w);
                    }
                }
            }
            cid += 1;
        }

        comp
    }

    /// Number of districts (c-components) in the ADMG.
    pub fn num_districts(&self) -> usize {
        self.districts().len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    fn setup() -> (EdgeRegistry, u8, u8) {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let bid = reg.code_of("<->").unwrap();
        (reg, dir, bid)
    }

    #[test]
    fn admg_basic_construction() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // 0 -> 1, 0 -> 2, 1 <-> 2, 2 -> 3
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(0, 2, dir).unwrap();
        b.add_edge(1, 2, bid).unwrap();
        b.add_edge(2, 3, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).expect("ADMG construction failed");
        assert_eq!(admg.n(), 4);
    }

    #[test]
    fn admg_parents_children_spouses() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // 0 -> 1, 0 -> 2, 1 <-> 2, 2 -> 3
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(0, 2, dir).unwrap();
        b.add_edge(1, 2, bid).unwrap();
        b.add_edge(2, 3, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Node 0: no parents, children {1, 2}, no spouses
        assert!(admg.parents_of(0).is_empty());
        assert_eq!(admg.children_of(0), &[1, 2]);
        assert!(admg.spouses_of(0).is_empty());

        // Node 1: parent {0}, no children, spouse {2}
        assert_eq!(admg.parents_of(1), &[0]);
        assert!(admg.children_of(1).is_empty());
        assert_eq!(admg.spouses_of(1), &[2]);

        // Node 2: parent {0}, child {3}, spouse {1}
        assert_eq!(admg.parents_of(2), &[0]);
        assert_eq!(admg.children_of(2), &[3]);
        assert_eq!(admg.spouses_of(2), &[1]);

        // Node 3: parent {2}, no children, no spouses
        assert_eq!(admg.parents_of(3), &[2]);
        assert!(admg.children_of(3).is_empty());
        assert!(admg.spouses_of(3).is_empty());
    }

    #[test]
    fn admg_neighbors() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // 0 -> 1, 0 <-> 2
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(0, 2, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        // Node 0 neighbors: [parents | spouses | children] = [| 2 | 1]
        assert_eq!(admg.neighbors_of(0), &[2, 1]);
    }

    #[test]
    fn admg_ancestors_descendants() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        // Chain: 0 -> 1 -> 2 -> 3, with 1 <-> 4 (spouse, not ancestor/descendant)
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        b.add_edge(2, 3, dir).unwrap();
        b.add_edge(1, 4, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Ancestors of 3: {0, 1, 2}
        assert_eq!(admg.ancestors_of(3), vec![0, 1, 2]);

        // Descendants of 0: {1, 2, 3}
        assert_eq!(admg.descendants_of(0), vec![1, 2, 3]);

        // Node 4's ancestors/descendants should be empty (only bidirected to 1)
        assert!(admg.ancestors_of(4).is_empty());
        assert!(admg.descendants_of(4).is_empty());
    }

    #[test]
    fn admg_markov_blanket() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        // 0 -> 1, 2 -> 1, 1 -> 3, 1 <-> 4
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(2, 1, dir).unwrap();
        b.add_edge(1, 3, dir).unwrap();
        b.add_edge(1, 4, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // MB(1) = Pa(1) ∪ Ch(1) ∪ Sp(1) ∪ coparents = {0, 2, 3, 4}
        assert_eq!(admg.markov_blanket_of(1), vec![0, 2, 3, 4]);
    }

    #[test]
    fn admg_exogenous() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // 0 -> 1, 2 <-> 3
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(2, 3, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Exogenous: nodes without parents = {0, 2, 3}
        assert_eq!(admg.exogenous_nodes(), vec![0, 2, 3]);
    }

    #[test]
    fn admg_cycle_rejected() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Directed cycle: 0 -> 1 -> 2 -> 0
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        b.add_edge(2, 0, dir).unwrap();

        let result = Admg::new(Arc::new(b.finalize().unwrap()));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("cycle"));
    }

    #[test]
    fn admg_undirected_edge_rejected() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let und = reg.code_of("---").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, und).unwrap(); // Undirected not allowed in ADMG

        let result = Admg::new(Arc::new(b.finalize().unwrap()));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("directed and bidirected"));
    }

    #[test]
    fn admg_empty_graph() {
        let (reg, _dir, _bid) = setup();
        let b = GraphBuilder::new_with_registry(3, true, &reg);
        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(admg.n(), 3);
        for i in 0..3 {
            assert!(admg.parents_of(i).is_empty());
            assert!(admg.children_of(i).is_empty());
            assert!(admg.spouses_of(i).is_empty());
        }
    }

    #[test]
    fn admg_only_bidirected() {
        let (reg, _dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Triangle of bidirected edges
        b.add_edge(0, 1, bid).unwrap();
        b.add_edge(1, 2, bid).unwrap();
        b.add_edge(0, 2, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        
        // All nodes have spouses but no parents/children
        assert!(admg.parents_of(0).is_empty());
        assert!(admg.children_of(0).is_empty());
        assert_eq!(admg.spouses_of(0), &[1, 2]);
    }

    #[test]
    fn admg_core_ref() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, dir).unwrap();
        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(admg.core_ref().n(), 2);
    }

    // ── District Tests ────────────────────────────────────────────────────────

    #[test]
    fn admg_districts_no_bidirected() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // Only directed edges: 0 -> 1 -> 2 -> 3
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        b.add_edge(2, 3, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        
        // Each node is its own district
        let districts = admg.districts();
        assert_eq!(districts.len(), 4);
        assert_eq!(admg.num_districts(), 4);
    }

    #[test]
    fn admg_districts_single_component() {
        let (reg, _dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // Chain of bidirected: 0 <-> 1 <-> 2 <-> 3
        b.add_edge(0, 1, bid).unwrap();
        b.add_edge(1, 2, bid).unwrap();
        b.add_edge(2, 3, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        
        // All nodes in one district
        let districts = admg.districts();
        assert_eq!(districts.len(), 1);
        assert_eq!(districts[0], vec![0, 1, 2, 3]);
        assert_eq!(admg.num_districts(), 1);
    }

    #[test]
    fn admg_districts_multiple_components() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(6, true, &reg);
        // District A: {0, 1} via 0 <-> 1
        // District B: {2, 3, 4} via 2 <-> 3, 3 <-> 4
        // District C: {5} (isolated)
        // Directed edges don't affect districts
        b.add_edge(0, 1, bid).unwrap();
        b.add_edge(2, 3, bid).unwrap();
        b.add_edge(3, 4, bid).unwrap();
        b.add_edge(0, 2, dir).unwrap(); // Directed doesn't connect districts

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        
        let districts = admg.districts();
        assert_eq!(districts.len(), 3);
        
        // Find district containing 0
        let d0 = admg.district_of(0);
        assert_eq!(d0, vec![0, 1]);
        
        // Find district containing 2
        let d2 = admg.district_of(2);
        assert_eq!(d2, vec![2, 3, 4]);
        
        // Find district containing 5
        let d5 = admg.district_of(5);
        assert_eq!(d5, vec![5]);
    }

    #[test]
    fn admg_district_membership() {
        let (reg, _dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // Two districts: {0, 1} and {2, 3}
        b.add_edge(0, 1, bid).unwrap();
        b.add_edge(2, 3, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();
        
        let membership = admg.district_membership();
        // Nodes in same district should have same membership
        assert_eq!(membership[0], membership[1]);
        assert_eq!(membership[2], membership[3]);
        assert_ne!(membership[0], membership[2]);
    }

    // ── M-Separation Tests ────────────────────────────────────────────────────

    #[test]
    fn admg_msep_chain() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Chain: 0 -> 1 -> 2
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // 0 and 2 are not m-separated unconditionally
        assert!(!admg.m_separated(&[0], &[2], &[]));

        // 0 and 2 are m-separated given 1
        assert!(admg.m_separated(&[0], &[2], &[1]));
    }

    #[test]
    fn admg_msep_collider() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Collider: 0 -> 2 <- 1
        b.add_edge(0, 2, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // 0 and 1 are m-separated unconditionally
        assert!(admg.m_separated(&[0], &[1], &[]));

        // 0 and 1 are NOT m-separated given 2 (collider opens)
        assert!(!admg.m_separated(&[0], &[1], &[2]));
    }

    #[test]
    fn admg_msep_bidirected_confounding() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // 0 -> 1, 0 <-> 2, 1 -> 2
        // This creates confounding: 0 <-> 2 represents latent confounder
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(0, 2, bid).unwrap();
        b.add_edge(1, 2, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // 0 and 2 are not m-separated (bidirected edge connects them)
        assert!(!admg.m_separated(&[0], &[2], &[]));

        // Even conditioning on 1 doesn't m-separate due to bidirected edge
        assert!(!admg.m_separated(&[0], &[2], &[1]));
    }

    #[test]
    fn admg_msep_pure_bidirected() {
        let (reg, _dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Chain of bidirected: 0 <-> 1 <-> 2
        // Represents latent confounders: L1 -> {0,1} and L2 -> {1,2}
        b.add_edge(0, 1, bid).unwrap();
        b.add_edge(1, 2, bid).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // M-separation uses the ancestral subgraph (through directed edges only).
        // Since there are no directed edges, An({0,2}) = {0,2} (node 1 is not included).
        // In this subgraph, 0 and 2 have no connecting edges, so they are m-separated.
        assert!(admg.m_separated(&[0], &[2], &[]));

        // When conditioning on 1, the ancestral subgraph includes An({0,1,2}) = {0,1,2}.
        // In this subgraph, the bidirected edges create moral edges 0-1 and 1-2.
        // But node 1 is blocked, so 0 and 2 are still m-separated.
        assert!(admg.m_separated(&[0], &[2], &[1]));
    }

    #[test]
    fn admg_msep_empty_sets() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Empty X or Y is trivially m-separated
        assert!(admg.m_separated(&[], &[1], &[]));
        assert!(admg.m_separated(&[0], &[], &[]));
    }

    #[test]
    fn admg_msep_iv_example() {
        let (reg, dir, bid) = setup();
        // Need simple=false to allow both directed and bidirected edges between same nodes
        let mut b = GraphBuilder::new_with_registry(3, false, &reg);
        // Instrumental variable setup:
        // Z -> X -> Y, X <-> Y (latent confounding)
        // 0:Z, 1:X, 2:Y
        b.add_edge(0, 1, dir).unwrap(); // Z -> X
        b.add_edge(1, 2, dir).unwrap(); // X -> Y
        b.add_edge(1, 2, bid).unwrap(); // X <-> Y (latent U)

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Z is m-separated from Y given X
        // The path Z -> X -> Y is blocked by X
        // The confounding X <-> Y doesn't create a path from Z to Y
        assert!(admg.m_separated(&[0], &[2], &[1]));

        // Z is not m-separated from Y unconditionally (path Z -> X -> Y)
        assert!(!admg.m_separated(&[0], &[2], &[]));
    }

    #[test]
    fn admg_msep_m_bias() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        // M-bias structure:
        // U1 -> X, U1 -> M, U2 -> M, U2 -> Y
        // 0:U1, 1:U2, 2:X, 3:M, 4:Y
        b.add_edge(0, 2, dir).unwrap(); // U1 -> X
        b.add_edge(0, 3, dir).unwrap(); // U1 -> M
        b.add_edge(1, 3, dir).unwrap(); // U2 -> M
        b.add_edge(1, 4, dir).unwrap(); // U2 -> Y

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // X and Y are m-separated unconditionally
        assert!(admg.m_separated(&[2], &[4], &[]));

        // X and Y are NOT m-separated given M (collider bias)
        assert!(!admg.m_separated(&[2], &[4], &[3]));
    }

    // ── Adjustment Tests ──────────────────────────────────────────────────────

    #[test]
    fn admg_adjustment_simple_confounding() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Classic confounding: L -> X -> Y, L -> Y
        // 0:L, 1:X, 2:Y
        b.add_edge(0, 1, dir).unwrap(); // L -> X
        b.add_edge(1, 2, dir).unwrap(); // X -> Y
        b.add_edge(0, 2, dir).unwrap(); // L -> Y

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Empty set is not valid (backdoor path L)
        assert!(!admg.is_valid_adjustment_set(&[1], &[2], &[]));

        // {L} is valid
        assert!(admg.is_valid_adjustment_set(&[1], &[2], &[0]));
    }

    #[test]
    fn admg_adjustment_latent_confounding() {
        let (reg, dir, bid) = setup();
        // Need simple=false to allow both directed and bidirected edges between same nodes
        let mut b = GraphBuilder::new_with_registry(3, false, &reg);
        // Latent confounding via bidirected: X -> Y, X <-> Y
        // 0:X, 1:Y, 2:other
        b.add_edge(0, 1, dir).unwrap(); // X -> Y
        b.add_edge(0, 1, bid).unwrap(); // X <-> Y (latent confounder)

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // No adjustment can block the bidirected confounding path
        // Empty set is not valid
        assert!(!admg.is_valid_adjustment_set(&[0], &[1], &[]));
    }

    #[test]
    fn admg_adjustment_front_door() {
        let (reg, dir, bid) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Front-door: X -> M -> Y, X <-> Y (latent U)
        // 0:X, 1:M, 2:Y
        b.add_edge(0, 1, dir).unwrap(); // X -> M
        b.add_edge(1, 2, dir).unwrap(); // M -> Y
        b.add_edge(0, 2, bid).unwrap(); // X <-> Y

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // In front-door setup, M is a valid adjustment for X -> Y
        // (through the front-door criterion, not backdoor)
        // Note: standard backdoor adjustment may not work here
        
        // M is a descendant of X, so it's forbidden for standard adjustment
        assert!(!admg.is_valid_adjustment_set(&[0], &[2], &[1]));
    }

    #[test]
    fn admg_adjustment_set_parents() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // L -> X -> Y, L -> Y, Z -> X
        // 0:L, 1:X, 2:Y, 3:Z
        b.add_edge(0, 1, dir).unwrap(); // L -> X
        b.add_edge(1, 2, dir).unwrap(); // X -> Y
        b.add_edge(0, 2, dir).unwrap(); // L -> Y
        b.add_edge(3, 1, dir).unwrap(); // Z -> X

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Parents of X are {L, Z}
        let adj = admg.adjustment_set_parents(&[1], &[2]);
        assert!(adj.is_some());
        let z = adj.unwrap();
        assert!(z.contains(&0)); // L
        assert!(z.contains(&3)); // Z
    }

    #[test]
    fn admg_adjustment_set_backdoor() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // L -> X -> Y, L -> Y
        // 0:L, 1:X, 2:Y, 3:isolated
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        b.add_edge(0, 2, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        let adj = admg.adjustment_set_backdoor(&[1], &[2]);
        assert!(adj.is_some());
        assert_eq!(adj.unwrap(), vec![0]); // Only L
    }

    #[test]
    fn admg_all_adjustment_sets() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // L -> X -> Y, L -> Y, M -> Y
        // 0:L, 1:X, 2:Y, 3:M
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        b.add_edge(0, 2, dir).unwrap();
        b.add_edge(3, 2, dir).unwrap();

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Get all minimal adjustment sets
        let sets = admg.all_adjustment_sets(&[1], &[2], true, 10);
        
        // {L} should be a minimal valid set
        assert!(sets.iter().any(|s| s == &vec![0]));
    }

    #[test]
    fn admg_adjustment_forbidden_descendant() {
        let (reg, dir, _bid) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // X -> M -> Y
        // 0:X, 1:M, 2:Y, 3:L (confounder)
        b.add_edge(0, 1, dir).unwrap(); // X -> M
        b.add_edge(1, 2, dir).unwrap(); // M -> Y
        b.add_edge(3, 0, dir).unwrap(); // L -> X
        b.add_edge(3, 2, dir).unwrap(); // L -> Y

        let admg = Admg::new(Arc::new(b.finalize().unwrap())).unwrap();

        // M is a descendant of X on causal path, so adjusting for M is invalid
        assert!(!admg.is_valid_adjustment_set(&[0], &[2], &[1]));

        // L is valid
        assert!(admg.is_valid_adjustment_set(&[0], &[2], &[3]));
    }
}
