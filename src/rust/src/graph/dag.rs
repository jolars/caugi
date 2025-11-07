// SPDX-License-Identifier: MIT
//! DAG wrapper with O(1) slice queries via packed neighborhoods.

use super::CaugiGraph;
use crate::edges::EdgeClass;
use crate::graph::alg::bitset;
use crate::graph::alg::csr;
use crate::graph::alg::directed_part_is_acyclic;
use crate::graph::alg::moral;
use crate::graph::pdag::Pdag;
use std::collections::{HashSet, VecDeque};
use crate::graph::ug::Ug;

use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Dag {
    core: Arc<CaugiGraph>,
    node_edge_ranges: Arc<[usize]>,
    node_deg: Arc<[(u32, u32)]>,
    neighborhoods: Arc<[u32]>,
}

impl Dag {
    /// Builds a `Dag` view over a class-agnostic CSR graph.
    ///
    /// Validates that the directed part is acyclic and that every edge is directed.
    /// Parents and children for each node are stored contiguously and are sorted.
    pub fn new(core: Arc<CaugiGraph>) -> Result<Self, String> {
        let n = core.n() as usize;

        if !directed_part_is_acyclic(&core) {
            return Err("Dag contains a directed cycle".into());
        }

        // Count `(parents, children)` per row.
        let mut deg: Vec<(u32, u32)> = vec![(0, 0); n];
        for i in 0..n {
            for k in core.row_range(i as u32) {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Directed => {
                        if core.side[k] == 1 {
                            deg[i].0 += 1 // parent
                        } else {
                            deg[i].1 += 1 // child
                        }
                    }
                    _ => return Err("Dag cannot contain non-directed edges".into()),
                }
            }
        }

        // Prefix sums for row slices into `neighborhoods`.
        let mut node_edge_ranges = Vec::with_capacity(n + 1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let (pa, ch) = deg[i];
            node_edge_ranges.push(node_edge_ranges[i] + (pa + ch) as usize);
        }
        let mut neigh = vec![0u32; *node_edge_ranges.last().unwrap()];

        // Single scatter pass per row.
        // Parents occupy the first segment, children the second.
        for i in 0..n {
            let start = node_edge_ranges[i];
            let end = node_edge_ranges[i + 1];
            let pa = deg[i].0 as usize;

            // Split the row slice once; fill with two cursors.
            let (pa_seg, ch_seg) = neigh[start..end].split_at_mut(pa);
            let mut pa_cur = 0;
            let mut ch_cur = 0;

            for k in core.row_range(i as u32) {
                let v = core.col_index[k];
                if core.side[k] == 1 {
                    pa_seg[pa_cur] = v;
                    pa_cur += 1;
                } else {
                    ch_seg[ch_cur] = v;
                    ch_cur += 1;
                }
            }
            debug_assert_eq!(pa_cur, pa);
            debug_assert_eq!(ch_cur, end - start - pa);
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

    /// Returns `(row_start, parents_end, children_start)` for node `i`.
    #[inline]
    fn row_bounds(&self, i: u32) -> (usize, usize, usize) {
        let i = i as usize;
        let start = self.node_edge_ranges[i];
        let end = self.node_edge_ranges[i + 1];
        let (pa, ch) = self.node_deg[i];
        (start, start + pa as usize, end - ch as usize)
    }

    /// Sorted slice of parents of `i` (borrowed view into packed storage).
    #[inline]
    pub fn parents_of(&self, i: u32) -> &[u32] {
        let (s, pmid, _) = self.row_bounds(i);
        &self.neighborhoods[s..pmid]
    }

    /// Sorted slice of children of `i` (borrowed view into packed storage).
    #[inline]
    pub fn children_of(&self, i: u32) -> &[u32] {
        let (_, _, cstart) = self.row_bounds(i);
        let e = self.node_edge_ranges[i as usize + 1];
        &self.neighborhoods[cstart..e]
    }

    /// Concatenated neighbors `[parents..., children...]` of `i`.
    #[inline]
    pub fn neighbors_of(&self, i: u32) -> &[u32] {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        &self.neighborhoods[s..e]
    }

    /// All ancestors of `i`, returned in ascending order.
    ///
    /// Implementation: DFS with a boolean mask for de-duplication, then scan.
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

    /// All descendants of `i`, returned in ascending order.
    ///
    /// Implementation: DFS with a boolean mask for de-duplication, then scan.
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

    /// Markov blanket of `i`: `Pa(i) ∪ Ch(i) ∪ (⋃ Pa(c) \ {i : c∈Ch(i)})`.
    #[inline]
    pub fn markov_blanket_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut m = vec![false; n];
        for &p in self.parents_of(i) {
            m[p as usize] = true;
        }
        for &c in self.children_of(i) {
            m[c as usize] = true;
            for &p in self.parents_of(c) {
                if p != i {
                    m[p as usize] = true;
                }
            }
        }
        m[i as usize] = false; // exclude self
        bitset::collect_from_mask(&m)
    }

    /// Nodes with no parents.
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

impl Dag {
    // -------- helpers for masks and small sets --------
    // These now delegate to the bitset and moral modules for reusability

    /// Collect ascending indices where `mask[i]` is `true`.
    #[inline]
    fn collect_from_mask(mask: &[bool]) -> Vec<u32> {
        bitset::collect_from_mask(mask)
    }

    /// Build a boolean membership mask for `nodes` over domain `[0, n)`.
    #[inline]
    fn mask_from(nodes: &[u32], n: u32) -> Vec<bool> {
        bitset::mask_from(nodes, n)
    }

    /// Ancestors mask of a seed set. `a[v] == true` iff `v ∈ An(seeds) ∪ seeds`.
    /// Used by d-separation and backdoor routines.
    fn ancestors_mask(&self, seeds: &[u32]) -> Vec<bool> {
        bitset::ancestors_mask(seeds, |u| self.parents_of(u), self.n())
    }

    /// Moralized adjacency within mask. Undirected edges among ancestors.
    /// Output adjacency lists are sorted and deduplicated.
    fn moral_adj(&self, mask: &[bool]) -> Vec<Vec<u32>> {
        moral::moral_adj(self.n(), |u| self.parents_of(u), mask)
    }

    /// BFS over moral graph to check reachability from `src` to any `tgt`
    /// while ignoring `blocked` nodes. Only visits nodes inside mask.
    fn reachable_to_any(
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

    /// Universe of candidates for backdoor adjustment wrt. `(x, y)`.
    /// Excludes `x`, `y`, and descendants of `x`.
    fn backdoor_universe(&self, x: u32, y: u32) -> Vec<u32> {
        let mut de_mask = vec![false; self.n() as usize];
        for d in self.descendants_of(x) {
            de_mask[d as usize] = true;
        }
        (0..self.n())
            .filter(|&v| v != x && v != y && !de_mask[v as usize])
            .collect()
    }

    /// Remove non-minimal supersets in-place, keep inclusion-minimal sets only.
    /// Input sets may be unsorted; this routine sorts and dedups each first.
    fn prune_minimal(sets: &mut Vec<Vec<u32>>) {
        sets.iter_mut().for_each(|v| {
            v.sort_unstable();
            v.dedup();
        });
        sets.sort();
        let mut out: Vec<Vec<u32>> = Vec::new();
        'next: for z in sets.drain(..) {
            for s in &out {
                if s.iter().all(|v| z.binary_search(v).is_ok()) {
                    continue 'next; // skip z if it is a superset of some s
                }
            }
            // Remove any existing superset of z.
            out.retain(|s| !z.iter().all(|v| s.binary_search(v).is_ok()));
            out.push(z);
        }
        *sets = out;
    }

    /// Enumerate all `k`-subsets of `u` into `out` (lexicographic order).
    fn k_subsets(u: &[u32], k: usize, start: usize, cur: &mut Vec<u32>, out: &mut Vec<Vec<u32>>) {
        if cur.len() == k {
            out.push(cur.clone());
            return;
        }
        for i in start..u.len() {
            cur.push(u[i]);
            Self::k_subsets(u, k, i + 1, cur, out);
            cur.pop();
        }
    }

    /// Backward reachability through parents from every `y ∈ ys`.
    /// Returns mask `r` with `r[v] == true` iff `v` can reach some `y`.
    fn can_reach_any_y(&self, ys: &[u32]) -> Vec<bool> {
        let mut r = vec![false; self.n() as usize];
        let mut st = ys.to_vec();
        while let Some(v) = st.pop() {
            let vi = v as usize;
            if r[vi] {
                continue;
            }
            r[vi] = true;
            st.extend_from_slice(self.parents_of(v));
        }
        r
    }

    /// Drop predicate for removing the first edge on each proper causal path.
    /// Uses `xs_mask` and a precomputed `reach_y` mask.
    fn drop_first_edge(&self, xs_mask: &[bool], reach_y: &[bool], row_u: u32, k: usize) -> bool {
        let c = self.core_ref();
        let v = c.col_index[k];
        if c.side[k] == 0 {
            // edge v -> row_u
            xs_mask[row_u as usize] && reach_y[v as usize]
        } else {
            // edge row_u -> v
            xs_mask[v as usize] && reach_y[row_u as usize]
        }
    }

    /// Rebuild a filtered CSR by dropping edges selected by predicate `drop(u, k)`.
    /// Preserves row order and metadata arrays.
    fn rebuild_filtered<F: FnMut(u32, usize) -> bool>(
        &self,
        mut drop: F,
    ) -> Result<CaugiGraph, String> {
        // Use the CSR filter_edges utility with inverted logic
        csr::filter_edges(self.core_ref(), |u, k, _c| !drop(u, k))
    }
}

impl Dag {
    /// Parent-based heuristic adjustment set:
    /// `Z = (⋃ Pa(X)) \ (X ∪ Y)`.
    pub fn adjustment_set_parents(&self, xs: &[u32], ys: &[u32]) -> Vec<u32> {
        let n = self.n();
        let mut keep = vec![false; n as usize];
        for &x in xs {
            for &p in self.parents_of(x) {
                keep[p as usize] = true;
            }
        }
        // Exclude X ∪ Y.
        let mut dropm = Self::mask_from(xs, n);
        for &y in ys {
            dropm[y as usize] = true;
        }
        for i in 0..keep.len() {
            if dropm[i] {
                keep[i] = false;
            }
        }
        Self::collect_from_mask(&keep)
    }

    /// Backdoor adjustment candidate set for `Xs → Ys`:
    /// `Z = ( (⋃ Pa(X)) ∩ An(Y) ) \ (De(X) ∪ X ∪ Y )`.
    ///
    /// Complexity: linear in the size of relevant neighborhoods.
    pub fn adjustment_set_backdoor(&self, xs: &[u32], ys: &[u32]) -> Vec<u32> {
        let n = self.n();
        // Mark An(Y).
        let an_mask = self.ancestors_mask(ys);

        // Mark parents of X that lie in An(Y).
        let mut keep = vec![false; n as usize];
        for &x in xs {
            for &p in self.parents_of(x) {
                if an_mask[p as usize] {
                    keep[p as usize] = true;
                }
            }
        }

        // Drop descendants of X, then drop X and Y.
        let mut dropm = vec![false; n as usize];
        for &x in xs {
            for d in self.descendants_of(x) {
                dropm[d as usize] = true;
            }
            dropm[x as usize] = true;
        }
        for &y in ys {
            dropm[y as usize] = true;
        }

        for i in 0..keep.len() {
            if dropm[i] {
                keep[i] = false;
            }
        }
        Self::collect_from_mask(&keep)
    }

    /// Optimal O-set for single exposure-outcome pair `x → y`.
    ///
    /// Definition:
    /// - Let `Cn = (De(x) ∩ An(y)) ∪ {y if y ∈ De(x)}`.
    /// - Return `Pa(Cn) \ (Cn ∪ {x})`.
    pub fn adjustment_set_optimal(&self, x: u32, y: u32) -> Vec<u32> {
        let n = self.n();
        // Mark descendants of x.
        let mut de_mask = vec![false; n as usize];
        for d in self.descendants_of(x) {
            de_mask[d as usize] = true;
        }
        // Mark ancestors of y.
        let an_mask = self.ancestors_mask(&[y]);

        // Cn mask: De(x) ∩ An(y) plus y if y ∈ De(x).
        let mut cn_mask = vec![false; n as usize];
        for i in 0..n as usize {
            if de_mask[i] && an_mask[i] {
                cn_mask[i] = true;
            }
        }
        if de_mask[y as usize] {
            cn_mask[y as usize] = true;
        }

        // Parents of Cn, excluding Cn and x.
        let mut pacn_mask = vec![false; n as usize];
        for v in Self::collect_from_mask(&cn_mask) {
            for &p in self.parents_of(v) {
                pacn_mask[p as usize] = true;
            }
        }
        pacn_mask[x as usize] = false;
        for i in 0..n as usize {
            if cn_mask[i] {
                pacn_mask[i] = false;
            }
        }
        Self::collect_from_mask(&pacn_mask)
    }

    /// d-separation test via ancestral reduction + moralization + BFS.
    ///
    /// Returns `true` iff every `x ∈ xs` is d-separated from every `y ∈ ys` given `z`.
    pub fn d_separated(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> bool {
        if xs.is_empty() || ys.is_empty() {
            return true;
        }
        let mut seeds = xs.to_vec();
        seeds.extend_from_slice(ys);
        seeds.extend_from_slice(z);
        seeds.sort_unstable();
        seeds.dedup();

        let mask = self.ancestors_mask(&seeds);

        // Block conditioned nodes.
        let mut blocked = vec![false; self.n() as usize];
        for &v in z {
            blocked[v as usize] = true;
        }

        let adj = self.moral_adj(&mask);
        !Self::reachable_to_any(&adj, &mask, xs, &blocked, ys)
    }

    /// Validates a proposed backdoor set `z` for pair `(x, y)`.
    ///
    /// Conditions:
    /// 1) `z` must not contain descendants of `x`.
    /// 2) Each parent `p` of `x` must be d-separated from `y` given `z ∪ {x}`.
    pub fn is_valid_backdoor_set(&self, x: u32, y: u32, z: &[u32]) -> bool {
        // Precompute De(x) mask for O(1) membership tests.
        let mut de_mask = vec![false; self.n() as usize];
        for d in self.descendants_of(x) {
            de_mask[d as usize] = true;
        }
        for &v in z {
            if de_mask[v as usize] {
                return false;
            }
        }

        // Test each parent of x.
        let mut obs = Vec::with_capacity(z.len() + 1);
        obs.extend_from_slice(z);
        obs.push(x);
        for &p in self.parents_of(x) {
            if !self.d_separated(&[p], &[y], &obs) {
                return false;
            }
        }
        true
    }

    /// Enumerate all valid backdoor sets up to size `max_size`.
    /// If `minimal` is true, return only inclusion-minimal sets.
    pub fn all_backdoor_sets(&self, x: u32, y: u32, minimal: bool, max_size: u32) -> Vec<Vec<u32>> {
        let u = self.backdoor_universe(x, y);
        let mut acc = Vec::new();
        let mut cur = Vec::new();
        let max_size = max_size as usize;
        for k in 0..=max_size.min(u.len()) {
            Self::k_subsets(&u, k, 0, &mut cur, &mut acc);
        }
        acc.retain(|z| self.is_valid_backdoor_set(x, y, z));
        if minimal {
            Self::prune_minimal(&mut acc);
        }
        acc
    }

    /// Build the proper backdoor graph for `Xs → Ys` by removing the
    /// first edge of each proper causal path from any `x ∈ Xs` to any `y ∈ Ys`.
    ///
    /// Implementation captures a precomputed `xs_mask` and a `reach_y` mask.
    pub fn proper_backdoor_graph(&self, xs: &[u32], ys: &[u32]) -> Result<Self, String> {
        let reach = self.can_reach_any_y(ys);
        let xs_mask = Self::mask_from(xs, self.n());
        let core = self.rebuild_filtered(|u, k| self.drop_first_edge(&xs_mask, &reach, u, k))?;
        Dag::new(Arc::new(core))
    }

    /// Validates `z` as an adjustment set for `Xs → Ys` using the proper backdoor graph.
    pub fn is_valid_adjustment_set(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> bool {
        self.proper_backdoor_graph(xs, ys)
            .map(|g| g.d_separated(xs, ys, z))
            .unwrap_or(false)
    }

    /// Build the proper backdoor graph core for `Xs → Ys`.
    /// This is the internal implementation used by public APIs.
    pub fn proper_backdoor_core(&self, xs: &[u32], ys: &[u32]) -> Result<CaugiGraph, String> {
        let reach = self.can_reach_any_y(ys);
        let xs_mask = Self::mask_from(xs, self.n());
        self.rebuild_filtered(|u, k| self.drop_first_edge(&xs_mask, &reach, u, k))
    }

    /// Build the moral graph of the entire DAG as an undirected graph core.
    pub fn moralize_core(&self) -> Result<CaugiGraph, String> {
        let mask = vec![true; self.n() as usize];
        let adj = self.moral_adj(&mask);
        csr::build_ug_core_from_adj(self.core_ref(), &adj)
    }

    /// Build the skeleton (undirected version) of the DAG as an undirected graph core.
    pub fn skeleton_core(&self) -> Result<CaugiGraph, String> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];
        for u in 0..self.n() {
            for &v in self.children_of(u) {
                adj[u as usize].push(v);
                adj[v as usize].push(u);
            }
        }
        // Sort and dedup
        for v in &mut adj {
            v.sort_unstable();
            v.dedup();
        }
        csr::build_ug_core_from_adj(self.core_ref(), &adj)
    }
}

impl Dag {
    pub fn to_cpdag(&self) -> Result<Pdag, String> {
        let n = self.n() as usize;

        // --- helpers ---
        let mut pa: Vec<HashSet<u32>> = vec![HashSet::new(); n];
        let mut ch: Vec<HashSet<u32>> = vec![HashSet::new(); n];
        let mut und: Vec<HashSet<u32>> = vec![HashSet::new(); n];

        #[inline]
        fn adjacent(
            a: usize,
            b: usize,
            und: &Vec<HashSet<u32>>,
            pa: &Vec<HashSet<u32>>,
            ch: &Vec<HashSet<u32>>,
        ) -> bool {
            und[a].contains(&(b as u32))
                || und[b].contains(&(a as u32))
                || pa[a].contains(&(b as u32))
                || ch[a].contains(&(b as u32))
                || pa[b].contains(&(a as u32))
                || ch[b].contains(&(a as u32))
        }
        #[inline]
        fn orient(
            a: u32,
            b: u32,
            und: &mut Vec<HashSet<u32>>,
            pa: &mut Vec<HashSet<u32>>,
            ch: &mut Vec<HashSet<u32>>,
        ) {
            let ai = a as usize;
            let bi = b as usize;
            und[ai].remove(&b);
            und[bi].remove(&a);
            ch[ai].insert(b);
            pa[bi].insert(a);
        }
        fn has_dir_path(ch: &Vec<HashSet<u32>>, src: u32, tgt: u32) -> bool {
            if src == tgt {
                return true;
            }
            let n = ch.len();
            let mut seen = vec![false; n];
            let mut q = VecDeque::new();
            q.push_back(src);
            while let Some(u) = q.pop_front() {
                if u == tgt {
                    return true;
                }
                if std::mem::replace(&mut seen[u as usize], true) {
                    continue;
                }
                for &v in &ch[u as usize] {
                    if !seen[v as usize] {
                        q.push_back(v);
                    }
                }
            }
            false
        }

        // --- skeleton from DAG (undirected) ---
        for u in 0..self.n() {
            for &v in self.children_of(u) {
                und[u as usize].insert(v);
                und[v as usize].insert(u);
            }
        }

        // --- orient v-structures: a->b<-c with a !~ c ---
        for b in 0..self.n() {
            let parents = self.parents_of(b).to_vec();
            for i in 0..parents.len() {
                for j in (i + 1)..parents.len() {
                    let a = parents[i] as usize;
                    let c = parents[j] as usize;
                    if !adjacent(a, c, &und, &pa, &ch) {
                        orient(parents[i], b, &mut und, &mut pa, &mut ch);
                        orient(parents[j], b, &mut und, &mut pa, &mut ch);
                    }
                }
            }
        }

        // --- Meek closure (R1–R4) ---
        loop {
            let mut changed = false;

            // R1: a->b, b--c, a !~ c  ⇒  b->c
            for b in 0..n {
                if pa[b].is_empty() || und[b].is_empty() {
                    continue;
                }
                let pb: Vec<u32> = pa[b].iter().copied().collect();
                let ubs: Vec<u32> = und[b].clone().into_iter().collect();
                'c_loop: for c in ubs {
                    let ci = c as usize;
                    for &a in &pb {
                        if !adjacent(a as usize, ci, &und, &pa, &ch) {
                            orient(b as u32, c, &mut und, &mut pa, &mut ch);
                            changed = true;
                            continue 'c_loop;
                        }
                    }
                }
            }

            // R2: a--b and ∃ w: a->w, w->b  ⇒  a->b
            for a in 0..n {
                let uab: Vec<u32> = und[a].clone().into_iter().collect();
                for b_u in uab {
                    let b = b_u as usize;
                    if ch[a].iter().any(|w| pa[b].contains(w)) {
                        orient(a as u32, b_u, &mut und, &mut pa, &mut ch);
                        changed = true;
                        continue;
                    }
                    if ch[b].iter().any(|w| pa[a].contains(w)) {
                        orient(b_u, a as u32, &mut und, &mut pa, &mut ch);
                        changed = true;
                    }
                }
            }

            // R3: a--b and ∃ c,d: c->b, d->b, c !~ d, a--c, a--d  ⇒  a->b
            for a in 0..n {
                let uab: Vec<u32> = und[a].clone().into_iter().collect();
                for b_u in uab {
                    let b = b_u as usize;
                    let pb: Vec<u32> = pa[b].iter().copied().collect();
                    'pairs: for i in 0..pb.len() {
                        for j in (i + 1)..pb.len() {
                            let c = pb[i] as usize;
                            let d = pb[j] as usize;
                            if !adjacent(c, d, &und, &pa, &ch)
                                && und[a].contains(&pb[i])
                                && und[a].contains(&pb[j])
                            {
                                orient(a as u32, b_u, &mut und, &mut pa, &mut ch);
                                changed = true;
                                break 'pairs;
                            }
                        }
                    }
                }
            }

            // R4: a--b and (a ⇒ b or b ⇒ a)  ⇒  orient along reachability
            for a in 0..n {
                let uab: Vec<u32> = und[a].clone().into_iter().collect();
                for b_u in uab {
                    if has_dir_path(&ch, a as u32, b_u) {
                        orient(a as u32, b_u, &mut und, &mut pa, &mut ch);
                        changed = true;
                    } else if has_dir_path(&ch, b_u, a as u32) {
                        orient(b_u, a as u32, &mut und, &mut pa, &mut ch);
                        changed = true;
                    }
                }
            }

            if !changed {
                break;
            }
        }

        // --- build CSR core (parents | undirected | children) ---
        let specs = &self.core_ref().registry.specs;
        let mut dir_code: Option<u8> = None;
        let mut und_code: Option<u8> = None;
        for (i, s) in specs.iter().enumerate() {
            match s.class {
                EdgeClass::Directed => {
                    if dir_code.is_none() || s.glyph == "-->" {
                        dir_code = Some(i as u8)
                    }
                }
                EdgeClass::Undirected => {
                    if und_code.is_none() || s.glyph == "---" {
                        und_code = Some(i as u8)
                    }
                }
                _ => {}
            }
        }
        let dir = dir_code.ok_or("No Directed edge spec in registry")?;
        let undc = und_code.ok_or("No Undirected edge spec in registry")?;

        let mut row_index = Vec::with_capacity(n + 1);
        row_index.push(0u32);
        for i in 0..n {
            let c = pa[i].len() + und[i].len() + ch[i].len();
            row_index.push(row_index[i] + c as u32);
        }
        let nnz = *row_index.last().unwrap() as usize;
        let mut col_index = vec![0u32; nnz];
        let mut etype = vec![0u8; nnz];
        let mut side = vec![0u8; nnz];

        let mut cur = row_index[..n].to_vec();
        for i in 0..n {
            // parents
            let mut v: Vec<u32> = pa[i].iter().copied().collect();
            v.sort_unstable();
            for p in v {
                let k = cur[i] as usize;
                col_index[k] = p;
                etype[k] = dir;
                side[k] = 1;
                cur[i] += 1;
            }
            // undirected
            let mut v: Vec<u32> = und[i].iter().copied().collect();
            v.sort_unstable();
            for u in v {
                let k = cur[i] as usize;
                col_index[k] = u;
                etype[k] = undc;
                side[k] = 0;
                cur[i] += 1;
            }
            // children
            let mut v: Vec<u32> = ch[i].iter().copied().collect();
            v.sort_unstable();
            for c in v {
                let k = cur[i] as usize;
                col_index[k] = c;
                etype[k] = dir;
                side[k] = 0;
                cur[i] += 1;
            }
        }

        let core = CaugiGraph::from_csr(
            row_index,
            col_index,
            etype,
            side,
            /*simple=*/ true,
            self.core_ref().registry.clone(),
        )?;
        Pdag::new(std::sync::Arc::new(core))
    }
}

impl Dag {
    fn undirected_code(&self) -> Result<u8, String> {
        let mut code: Option<u8> = None;
        for (i, s) in self.core_ref().registry.specs.iter().enumerate() {
            if let EdgeClass::Undirected = s.class {
                if code.is_none() || s.glyph == "---" {
                    code = Some(i as u8);
                }
            }
        }
        code.ok_or("No Undirected edge spec in registry".into())
    }

    fn build_ug_core_from_adj(&self, adj: &[Vec<u32>]) -> Result<CaugiGraph, String> {
        let n = self.n() as usize;
        let und = self.undirected_code()?;

        let mut row_index = Vec::with_capacity(n + 1);
        row_index.push(0u32);
        for i in 0..n {
            row_index.push(row_index[i] + adj[i].len() as u32);
        }
        let nnz = *row_index.last().unwrap() as usize;

        let mut col_index = vec![0u32; nnz];
        let etype = vec![und; nnz];
        let side = vec![0u8; nnz];

        let mut cur = row_index[..n].to_vec();
        for i in 0..n {
            for &v in &adj[i] {
                let k = cur[i] as usize;
                col_index[k] = v;
                cur[i] += 1;
            }
        }

        CaugiGraph::from_csr(
            row_index,
            col_index,
            etype,
            side,
            /*simple=*/ true,
            self.core_ref().registry.clone(),
        )
    }

    /// UG skeleton: undirect every edge in the DAG.
    pub fn skeleton(&self) -> Result<Ug, String> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];
        for i in 0..n {
            let mut v = self.neighbors_of(i as u32).to_vec(); // Pa ∪ Ch
            v.sort_unstable();
            v.dedup();
            adj[i] = v;
        }
        let core = self.build_ug_core_from_adj(&adj)?;
        Ug::new(Arc::new(core))
    }

    /// UG moral graph: undirect all edges and marry parents.
    pub fn moralize(&self) -> Result<Ug, String> {
        let mask = vec![true; self.n() as usize];
        let adj = self.moral_adj(&mask); // already sorted+dedup
        let core = self.build_ug_core_from_adj(&adj)?;
        Ug::new(Arc::new(core))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn dag_relations() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(0, 2, cdir).unwrap();
        b.add_edge(3, 0, cdir).unwrap();

        let core = std::sync::Arc::new(b.finalize().unwrap());
        let dag = Dag::new(core).expect("Dag construction failed");
        assert_eq!(dag.children_of(0), vec![1, 2]);
        assert_eq!(dag.parents_of(0), vec![3]);
        assert_eq!(dag.parents_of(1), vec![0]);
        assert!(dag.children_of(1).is_empty());
        assert_eq!(dag.children_of(3), vec![0]);
        assert_eq!(dag.neighbors_of(0), vec![3, 1, 2]);
        assert_eq!(dag.neighbors_of(1), vec![0]);
        assert_eq!(dag.neighbors_of(2), vec![0]);
        assert_eq!(dag.neighbors_of(3), vec![0]);
        assert_eq!(dag.n(), 4);

        // get core
        let core = dag.core_ref();
        assert_eq!(core.n(), 4);
    }

    #[test]
    fn dag_an_de() {
        // 3 -> 0 -> {1,2}
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let c = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(3, 0, c).unwrap();
        b.add_edge(0, 1, c).unwrap();
        b.add_edge(0, 2, c).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.ancestors_of(0), vec![3]);
        assert_eq!(g.descendants_of(0), vec![1, 2]);
        assert_eq!(g.ancestors_of(2), vec![0, 3]);
        assert_eq!(g.descendants_of(3), vec![0, 1, 2]);
    }

    #[test]
    fn dag_mb() {
        // 2 -> 1 <- 0 -> 3
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 3, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.markov_blanket_of(0), vec![1, 2, 3]); // pa(0)=∅, ch(0)={1,3}, spouses via 1={2}
    }

    #[test]
    fn dag_cycle_rejected() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cdir).unwrap();
        b.add_edge(2, 0, cdir).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let dag = Dag::new(core);
        assert!(dag.is_err());
    }

    #[test]
    fn dag_non_directed_rejected() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let cund = reg.code_of("<->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let dag = Dag::new(core);
        assert!(dag.is_err());
    }

    #[test]
    fn dag_exogenous() {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        // 0->1, 0->2; node 3 isolated => exogenous {0,3}
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.exogenous_nodes(), vec![0, 3]);
    }

    #[test]
    fn dag_adjustment_parents_basic() {
        // 0 -> 1; 2 -> 1. For X={1}, Y={2}, parents(X)\{X∪Y} = {0}.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(2, 1, d).unwrap();

        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.adjustment_set_parents(&[1], &[2]), vec![0]);
    }

    #[test]
    fn dag_adjustment_backdoor_multi_x() {
        // 0->2, 1->2, 2->3; X={0,1}, Y={3}. No backdoor via parents(X)∩An(Y) => ∅.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(2, 3, d).unwrap();

        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.adjustment_set_backdoor(&[0, 1], &[3]).is_empty());
    }

    #[test]
    fn dag_optimal_adjustment_confounder_and_instrument() {
        // L -> A -> Y; L -> Y  => optimal set {L}
        // Z -> A -> Y          => instrument Z should NOT appear
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        // nodes: 0:L, 1:A, 2:Y, 3:Z
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, d).unwrap(); // L->A
        b.add_edge(1, 2, d).unwrap(); // A->Y
        b.add_edge(0, 2, d).unwrap(); // L->Y
        b.add_edge(3, 1, d).unwrap(); // Z->A (instrument)

        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.adjustment_set_optimal(1, 2), vec![0]); // only L
    }

    #[test]
    fn dag_d_separated_basic_patterns() {
        // Chain: 0->1->2. 0 ⟂̸ 2 | ∅ ; but 0 ⟂ 2 | {1}.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        assert_eq!(g.d_separated(&[0], &[2], &[]), false);
        assert_eq!(g.d_separated(&[0], &[2], &[1]), true);
    }

    #[test]
    fn dag_d_separated_collider_activation() {
        // Collider: 0->2<-1. 0 ⟂ 1 | ∅ ; but 0 ⟂̸ 1 | {2}.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        assert_eq!(g.d_separated(&[0], &[1], &[]), true);
        assert_eq!(g.d_separated(&[0], &[1], &[2]), false);
    }

    #[test]
    fn dag_is_valid_backdoor_and_all_backdoor_sets() {
        // L -> A -> Y; L -> Y. Valid backdoor set is {L}; empty set invalid.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        // 0:L, 1:A, 2:Y
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(0, 2, d).unwrap();

        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.is_valid_backdoor_set(1, 2, &[]), false);
        assert_eq!(g.is_valid_backdoor_set(1, 2, &[0]), true);

        let mut sets = g.all_backdoor_sets(1, 2, true, 20);
        // Expect exactly one minimal set: {L}
        sets.sort();
        assert_eq!(sets, vec![vec![0]]);
    }

    #[test]
    fn dag_is_valid_adjustment_set_via_proper_backdoor_graph() {
        // Same confounding example: {L} should be valid for X={A}, Y={Y}; empty invalid.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        // 0:L, 1:A, 2:Y
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(0, 2, d).unwrap();

        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.is_valid_adjustment_set(&[1], &[2], &[]), false);
        assert_eq!(g.is_valid_adjustment_set(&[1], &[2], &[0]), true);
    }

    #[test]
    fn dag_d_separated_empty_x_or_y_trivially_true() {
        // With empty X or Y, should return true by definition.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        assert!(g.d_separated(&[], &[1], &[]));
        assert!(g.d_separated(&[0], &[], &[]));
    }
    #[test]
    fn dag_descendants_seen_continue_path() {
        // Diamond: 0 -> 1, 0 -> 2, 1 -> 3, 2 -> 3
        // descendants_of(0) pushes 3 twice, exercising the `seen`-continue branch.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 3, d).unwrap();
        b.add_edge(2, 3, d).unwrap();

        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.descendants_of(0), vec![1, 2, 3]);
    }

    #[test]
    fn dag_adjustment_backdoor_intersection_hits_equal_branch() {
        // 0 -> 1 -> 2 and 3 -> 1. For X={1}, Y={2}:
        // Pa(X)={0,3}, An(Y)={0,1,3} => intersection {0,3} (exercises Equal branch).
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(3, 1, d).unwrap();

        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.adjustment_set_backdoor(&[1], &[2]), vec![0, 3]);
    }

    #[test]
    fn dag_all_backdoor_sets_prunes_supersets() {
        // Two confounders: L and M each create a backdoor path A<-L->Y and A<-M->Y.
        // To block *both* backdoors you must adjust for {L, M}; {L} or {M} alone is insufficient.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        // 0:L, 1:M, 2:A, 3:Y
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 2, d).unwrap(); // L->A
        b.add_edge(1, 2, d).unwrap(); // M->A
        b.add_edge(2, 3, d).unwrap(); // A->Y
        b.add_edge(0, 3, d).unwrap(); // L->Y
        b.add_edge(1, 3, d).unwrap(); // M->Y

        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        let mut sets = dag.all_backdoor_sets(2, 3, true, 20);
        sets.sort();
        assert_eq!(sets, vec![vec![0, 1]]); // minimal set is {L, M}
    }

    #[test]
    fn dag_adjustment_parents_excludes_x_and_y() {
        // 0 -> 1, 2 -> 1, 2 -> 3.
        // parents(X={1}) = {0,2}; excluding X∪Y with Y={3} keeps {0,2}.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(2, 3, d).unwrap();

        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(dag.adjustment_set_parents(&[1], &[3]), vec![0, 2]);
    }

    #[test]
    fn dag_proper_backdoor_graph_does_not_block_backdoor_by_itself() {
        // L -> A -> Y and L -> Y.
        // Proper backdoor graph removes A->Y (first edge on causal path from A to Y),
        // but backdoor path A <- L -> Y remains, so A and Y are NOT d-separated without conditioning.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        // 0:L, 1:A, 2:Y
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap(); // L->A
        b.add_edge(1, 2, d).unwrap(); // A->Y
        b.add_edge(0, 2, d).unwrap(); // L->Y

        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        let pb = dag.proper_backdoor_graph(&[1], &[2]).unwrap();

        assert_eq!(pb.parents_of(2), &[0]); // A->Y removed; L->Y kept
        assert!(!pb.d_separated(&[1], &[2], &[])); // A and Y still d-connected via L
    }

    #[test]
    fn dag_prune_minimal_skips_supersets_branch() {
        // out starts with {0}; incoming {0,1} should be skipped by the 'continue \'next;'
        let mut sets = vec![vec![0], vec![0, 1]];
        Dag::prune_minimal(&mut sets);
        assert_eq!(sets, vec![vec![0]]);
    }

    #[test]
    fn dag_prune_minimal_removes_existing_supersets_when_subset_arrives() {
        // {0,1} is accepted first; then {0} should remove it via the retain() line
        let mut sets = vec![vec![0, 1], vec![0]];
        Dag::prune_minimal(&mut sets);
        assert_eq!(sets, vec![vec![0]]);
    }

    #[test]
    fn dag_moral_adj_skips_parents_not_in_mask() {
        // Graph: 0 -> 2, 1 -> 2. We'll exclude parent 1 from the mask.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Mask 'a': include v=2 and parent 0; exclude parent 1.
        let mut a = vec![false; dag.n() as usize];
        a[2] = true; // node v
        a[0] = true; // parent included
        // a[1] stays false (parent excluded)

        // Call the private helper directly (allowed inside this module's tests).
        let adj = dag.moral_adj(&a);

        // Expect only edges among included nodes: (2)-(0). No edges touching node 1.
        assert_eq!(adj[2], vec![0]);
        assert_eq!(adj[0], vec![2]);
        assert!(adj[1].is_empty());
    }

    // test to_cpdag
    #[test]
    fn dag_to_cpdag_basic() {
        // Graph: 0 -> 1, 2 -> 1
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let cpdag = dag.to_cpdag().unwrap();

        // Check edges in CPDAG
        // Expect directed edges: 0 -> 1, 2 -> 1
        assert_eq!(cpdag.parents_of(1), vec![0, 2]);
        assert_eq!(cpdag.children_of(0), vec![1]);
        assert_eq!(cpdag.children_of(2), vec![1]);
    }

        #[test]
    fn dag_skeleton_basic() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        let ug = dag.skeleton().unwrap();
        assert_eq!(ug.neighbors_of(0), &[1, 2]);
        assert_eq!(ug.neighbors_of(1), &[0]);
        assert_eq!(ug.neighbors_of(2), &[0]);
    }

    #[test]
    fn dag_moralize_married_parents() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        let ug = dag.moralize().unwrap();
        assert_eq!(ug.neighbors_of(0), &[1, 2]);
        assert_eq!(ug.neighbors_of(1), &[0, 2]);
        assert_eq!(ug.neighbors_of(2), &[0, 1]);
    }
}
