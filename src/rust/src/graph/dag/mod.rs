// SPDX-License-Identifier: MIT
//! DAG wrapper with O(1) slice queries via packed neighborhoods.

mod adjustment;
mod transforms;

use super::error::DagError;
use super::CaugiGraph;
use crate::edges::EdgeClass;
use crate::graph::alg::bitset;
use crate::graph::alg::csr;
use crate::graph::alg::directed_part_is_acyclic;
use crate::graph::alg::moral;
use crate::graph::alg::traversal;
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
    ///
    /// Returns a `String` error for FFI compatibility. Use `try_new` for typed errors.
    pub fn new(core: Arc<CaugiGraph>) -> Result<Self, String> {
        Self::try_new(core).map_err(|e| e.to_string())
    }

    /// Builds a `Dag` view with typed error handling.
    ///
    /// See [`new`](Self::new) for details.
    pub fn try_new(core: Arc<CaugiGraph>) -> Result<Self, DagError> {
        let n = core.n() as usize;

        if !directed_part_is_acyclic(&core) {
            return Err(DagError::DirectedCycle);
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
                    _ => {
                        return Err(DagError::InvalidEdgeType {
                            found: spec.glyph.clone(),
                        })
                    }
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
    #[inline]
    pub fn ancestors_of(&self, i: u32) -> Vec<u32> {
        traversal::ancestors_of(self.n(), i, |u| self.parents_of(u))
    }

    /// All descendants of `i`, returned in ascending order.
    #[inline]
    pub fn descendants_of(&self, i: u32) -> Vec<u32> {
        traversal::descendants_of(self.n(), i, |u| self.children_of(u))
    }

    /// Markov blanket of `i`: `Pa(i) ∪ Ch(i) ∪ (⋃ Pa(c) \ {i : c∈Ch(i)})`.
    #[inline]
    pub fn markov_blanket_of(&self, i: u32) -> Vec<u32> {
        traversal::markov_blanket_dag(self.n(), i, |u| self.parents_of(u), |u| self.children_of(u))
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

// -------- Internal helpers for masks and graph operations --------
impl Dag {
    /// Collect ascending indices where `mask[i]` is `true`.
    #[inline]
    pub(crate) fn collect_from_mask(mask: &[bool]) -> Vec<u32> {
        bitset::collect_from_mask(mask)
    }

    /// Build a boolean membership mask for `nodes` over domain `[0, n)`.
    #[inline]
    pub(crate) fn mask_from(nodes: &[u32], n: u32) -> Vec<bool> {
        bitset::mask_from(nodes, n)
    }

    /// Ancestors mask of a seed set. `a[v] == true` iff `v ∈ An(seeds) ∪ seeds`.
    pub(crate) fn ancestors_mask(&self, seeds: &[u32]) -> Vec<bool> {
        bitset::ancestors_mask(seeds, |u| self.parents_of(u), self.n())
    }

    /// Descendants mask of a seed set. `d[v] == true` iff `v ∈ De(seeds) ∪ seeds`.
    pub(crate) fn descendants_mask(&self, seeds: &[u32]) -> Vec<bool> {
        bitset::descendants_mask(seeds, |u| self.children_of(u), self.n())
    }

    /// Moralized adjacency within mask. Undirected edges among ancestors.
    pub(crate) fn moral_adj(&self, mask: &[bool]) -> Vec<Vec<u32>> {
        moral::moral_adj(self.n(), |u| self.parents_of(u), mask)
    }

    /// BFS over moral graph to check reachability from `src` to any `tgt`
    /// while ignoring `blocked` nodes. Only visits nodes inside mask.
    pub(crate) fn reachable_to_any(
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

    /// Backward reachability through parents from every `y ∈ ys`.
    pub(crate) fn can_reach_any_y(&self, ys: &[u32]) -> Vec<bool> {
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
    pub(crate) fn drop_first_edge(
        &self,
        xs_mask: &[bool],
        reach_y: &[bool],
        row_u: u32,
        k: usize,
    ) -> bool {
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
    pub(crate) fn rebuild_filtered<F: FnMut(u32, usize) -> bool>(
        &self,
        mut drop: F,
    ) -> Result<CaugiGraph, String> {
        csr::filter_edges(self.core_ref(), |u, k, _c| !drop(u, k))
    }

    /// Remove non-minimal supersets in-place, keep inclusion-minimal sets only.
    pub(crate) fn prune_minimal(sets: &mut Vec<Vec<u32>>) {
        sets.iter_mut().for_each(|v| {
            v.sort_unstable();
            v.dedup();
        });
        sets.sort();
        let mut out: Vec<Vec<u32>> = Vec::new();
        'next: for z in sets.drain(..) {
            for s in &out {
                if s.iter().all(|v| z.binary_search(v).is_ok()) {
                    continue 'next;
                }
            }
            out.retain(|s| !z.iter().all(|v| s.binary_search(v).is_ok()));
            out.push(z);
        }
        *sets = out;
    }

    /// Enumerate all `k`-subsets of `u` into `out` (lexicographic order).
    pub(crate) fn k_subsets(
        u: &[u32],
        k: usize,
        start: usize,
        cur: &mut Vec<u32>,
        out: &mut Vec<Vec<u32>>,
    ) {
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

        let core = dag.core_ref();
        assert_eq!(core.n(), 4);
    }

    #[test]
    fn dag_an_de() {
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
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 3, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.markov_blanket_of(0), vec![1, 2, 3]);
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
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.exogenous_nodes(), vec![0, 3]);
    }

    #[test]
    fn dag_prune_minimal_skips_supersets_branch() {
        let mut sets = vec![vec![0], vec![0, 1]];
        Dag::prune_minimal(&mut sets);
        assert_eq!(sets, vec![vec![0]]);
    }

    #[test]
    fn dag_prune_minimal_removes_existing_supersets_when_subset_arrives() {
        let mut sets = vec![vec![0, 1], vec![0]];
        Dag::prune_minimal(&mut sets);
        assert_eq!(sets, vec![vec![0]]);
    }

    #[test]
    fn dag_descendants_seen_continue_path() {
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
}

