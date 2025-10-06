// SPDX-License-Identifier: MIT
//! DAG wrapper with O(1) slice queries via packed neighborhoods.

use super::CaugiGraph;
use crate::edges::EdgeClass;
use crate::graph::alg::directed_part_is_acyclic;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Dag {
    core: Arc<CaugiGraph>,
    node_edge_ranges: Arc<[usize]>,
    node_deg: Arc<[(u32, u32)]>,
    neighbourhoods: Arc<[u32]>,
}

impl Dag {
    pub fn new(core: Arc<CaugiGraph>) -> Result<Self, String> {
        let n = core.n() as usize;

        if !directed_part_is_acyclic(&core) {
            return Err("Dag contains a directed cycle".into());
        }

        // count parents, children per row
        let mut deg: Vec<(u32, u32)> = vec![(0, 0); n];
        for i in 0..n {
            for k in core.row_range(i as u32) {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Directed => {
                        if core.side[k] == 1 {
                            deg[i].0 += 1
                        } else {
                            deg[i].1 += 1
                        }
                    }
                    _ => return Err("Dag cannot contain non-directed edges".into()),
                }
            }
        }

        // prefix sums
        let mut node_edge_ranges = Vec::with_capacity(n + 1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let (pa, ch) = deg[i];
            node_edge_ranges.push(node_edge_ranges[i] + (pa + ch) as usize);
        }
        let mut neigh = vec![0u32; *node_edge_ranges.last().unwrap()];

        // single scatter pass
        for i in 0..n {
            let start = node_edge_ranges[i];
            let end = node_edge_ranges[i + 1];
            let pa = deg[i].0 as usize;

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
            neighbourhoods: neigh.into(),
        })
    }

    #[inline]
    pub fn n(&self) -> u32 {
        self.core.n()
    }
    #[inline]
    fn row_bounds(&self, i: u32) -> (usize, usize, usize) {
        let i = i as usize;
        let start = self.node_edge_ranges[i];
        let end = self.node_edge_ranges[i + 1];
        let (pa, ch) = self.node_deg[i];
        (start, start + pa as usize, end - ch as usize) // (start, parents_end, children_start)
    }

    #[inline]
    pub fn parents_of(&self, i: u32) -> &[u32] {
        let (s, pmid, _) = self.row_bounds(i);
        &self.neighbourhoods[s..pmid]
    }

    #[inline]
    pub fn children_of(&self, i: u32) -> &[u32] {
        let (_, _, cstart) = self.row_bounds(i);
        let e = self.node_edge_ranges[i as usize + 1];
        &self.neighbourhoods[cstart..e]
    }

    #[inline]
    pub fn neighbors_of(&self, i: u32) -> &[u32] {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        &self.neighbourhoods[s..e]
    }

    #[inline]
    pub fn ancestors_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut out = Vec::new();
        let mut stack: Vec<u32> = self.parents_of(i).to_vec();
        while let Some(u) = stack.pop() {
            let ui = u as usize;
            if seen[ui] {
                continue;
            }
            seen[ui] = true;
            out.push(u);
            stack.extend_from_slice(self.parents_of(u));
        }
        out.sort_unstable();
        out
    }
    #[inline]
    pub fn descendants_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut out = Vec::new();
        let mut stack: Vec<u32> = self.children_of(i).to_vec();
        while let Some(u) = stack.pop() {
            let ui = u as usize;
            if seen[ui] {
                continue;
            }
            seen[ui] = true;
            out.push(u);
            stack.extend_from_slice(self.children_of(u));
        }
        out.sort_unstable();
        out
    }
    #[inline]
    pub fn markov_blanket_of(&self, i: u32) -> Vec<u32> {
        let mut mb: Vec<u32> = Vec::new();
        mb.extend_from_slice(self.parents_of(i));
        mb.extend_from_slice(self.children_of(i));
        for &c in self.children_of(i) {
            for &p in self.parents_of(c) {
                if p != i {
                    mb.push(p);
                }
            }
        }
        mb.sort_unstable();
        mb.dedup();
        mb
    }
    #[inline]
    pub fn exogenous_nodes(&self) -> Vec<u32> {
        (0..self.n())
            .filter(|&i| self.parents_of(i).is_empty())
            .collect()
    }

    pub fn core_ref(&self) -> &CaugiGraph {
        &self.core
    }
}

impl Dag {
    // -------- small set helpers --------
    #[inline]
    fn dedup(v: &mut Vec<u32>) {
        v.sort_unstable();
        v.dedup();
    }
    #[inline]
    fn bitset(&self) -> Vec<bool> {
        vec![false; self.n() as usize]
    }
    #[inline]
    fn mark(mask: &mut [bool], vs: &[u32]) {
        for &v in vs {
            mask[v as usize] = true;
        }
    }

    #[inline]
    fn intersect_sorted(a: &[u32], b: &[u32]) -> Vec<u32> {
        let (mut i, mut j) = (0, 0);
        let mut out = Vec::new();
        while i < a.len() && j < b.len() {
            use std::cmp::Ordering::*;
            match a[i].cmp(&b[j]) {
                Less => i += 1,
                Greater => j += 1,
                Equal => {
                    out.push(a[i]);
                    i += 1;
                    j += 1;
                }
            }
        }
        out
    }
    #[inline]
    fn union_sorted<I: IntoIterator<Item = Vec<u32>>>(parts: I) -> Vec<u32> {
        let mut out = Vec::new();
        for mut p in parts {
            Self::dedup(&mut p);
            out.extend_from_slice(&p);
        }
        Self::dedup(&mut out);
        out
    }
    #[inline]
    fn difference_sorted(a: &[u32], b: &[u32]) -> Vec<u32> {
        let mut out = Vec::with_capacity(a.len());
        let mut j = 0;
        for &v in a {
            while j < b.len() && b[j] < v {
                j += 1;
            }
            if j == b.len() || b[j] != v {
                out.push(v);
            }
        }
        out
    }

    pub fn adjustment_set_parents(&self, xs: &[u32], ys: &[u32]) -> Vec<u32> {
        let pax = Self::union_sorted(xs.iter().map(|&x| self.parents_of(x).to_vec()));
        let mut excl = xs.to_vec();
        excl.extend_from_slice(ys);
        Self::dedup(&mut excl);
        Self::difference_sorted(&pax, &excl)
    }

    // -------- public: candidate Z via Pearl’s backdoor formula --------

    /// Z = ( (⋃ Pa(X)) ∩ An(Y) ) \ (De(X) ∪ X ∪ Y)
    pub fn adjustment_set_backdoor(&self, xs: &[u32], ys: &[u32]) -> Vec<u32> {
        let pax = Self::union_sorted(xs.iter().map(|&x| self.parents_of(x).to_vec()));
        let any = Self::union_sorted(ys.iter().map(|&y| self.ancestors_of(y)));
        let inter = Self::intersect_sorted(&pax, &any);

        let mut excl = Self::union_sorted(xs.iter().map(|&x| self.descendants_of(x)));
        excl.extend_from_slice(xs);
        excl.extend_from_slice(ys);
        Self::dedup(&mut excl);

        Self::difference_sorted(&inter, &excl)
    }

    /// O-set for single X→Y
    pub fn adjustment_set_optimal(&self, x: u32, y: u32) -> Vec<u32> {
        let mut de = self.descendants_of(x);
        Self::dedup(&mut de);
        let mut an = self.ancestors_of(y);
        Self::dedup(&mut an);

        let mut cn = Self::intersect_sorted(&de, &an);
        if de.binary_search(&y).is_ok() {
            cn.push(y);
        }
        Self::dedup(&mut cn);

        let pacn = Self::union_sorted(cn.iter().map(|&v| self.parents_of(v).to_vec()));
        let mut forbid = cn;
        forbid.push(x);
        Self::dedup(&mut forbid);

        Self::difference_sorted(&pacn, &forbid)
    }

    // -------- d-separation (ancestral reduction + moralization) --------
    fn ancestors_mask(&self, seeds: &[u32]) -> Vec<bool> {
        let mut a = self.bitset();
        let mut st = Vec::new();
        for &s in seeds {
            if !a[s as usize] {
                a[s as usize] = true;
                st.extend_from_slice(self.parents_of(s));
            }
        }
        while let Some(u) = st.pop() {
            let ui = u as usize;
            if a[ui] {
                continue;
            }
            a[ui] = true;
            st.extend_from_slice(self.parents_of(u));
        }
        a
    }
    fn moral_adj(&self, a: &[bool]) -> Vec<Vec<u32>> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];
        for v in 0..self.n() {
            if !a[v as usize] {
                continue;
            }
            let pa = self.parents_of(v);
            for &p in pa {
                if a[p as usize] {
                    adj[v as usize].push(p);
                    adj[p as usize].push(v);
                }
            }
            for i in 0..pa.len() {
                let pi = pa[i] as usize;
                if !a[pi] {
                    continue;
                }
                for j in i + 1..pa.len() {
                    let pj = pa[j] as usize;
                    if !a[pj] {
                        continue;
                    }
                    adj[pi].push(pa[j]);
                    adj[pj].push(pa[i]);
                }
            }
        }
        for v in &mut adj {
            Self::dedup(v);
        }
        adj
    }
    fn reachable_to_any(
        adj: &[Vec<u32>],
        a: &[bool],
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
            if a[xi] && !blocked[xi] && !seen[xi] {
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
                if a[wi] && !blocked[wi] && !seen[wi] {
                    seen[wi] = true;
                    q.push_back(w);
                }
            }
        }
        false
    }
    /// Returns true iff every x∈xs is d-separated from every y∈ys given z.
    pub fn is_d_separated(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> bool {
        if xs.is_empty() || ys.is_empty() {
            return true;
        }
        let mut seeds = xs.to_vec();
        seeds.extend_from_slice(ys);
        seeds.extend_from_slice(z);
        Self::dedup(&mut seeds);
        let a = self.ancestors_mask(&seeds);
        let mut blocked = self.bitset();
        Self::mark(&mut blocked, z);
        let adj = self.moral_adj(&a);
        !Self::reachable_to_any(&adj, &a, xs, &blocked, ys)
    }

    // -------- backdoor utilities --------
    pub fn is_valid_backdoor_set(&self, x: u32, y: u32, z: &[u32]) -> bool {
        let mut de = self.descendants_of(x);
        de.sort_unstable();
        de.dedup();
        for &v in z {
            if de.binary_search(&v).is_ok() {
                return false;
            }
        }
        let mut obs = Vec::with_capacity(z.len() + 1);
        obs.extend_from_slice(z);
        obs.push(x);
        for &p in self.parents_of(x) {
            if !self.is_d_separated(&[p], &[y], &obs) {
                return false;
            }
        }
        true
    }
    fn backdoor_universe(&self, x: u32, y: u32) -> Vec<u32> {
        let mut de = self.descendants_of(x);
        Self::dedup(&mut de);
        (0..self.n())
            .filter(|&v| v != x && v != y && de.binary_search(&v).is_err())
            .collect()
    }
    fn prune_minimal(sets: &mut Vec<Vec<u32>>) {
        sets.iter_mut().for_each(Self::dedup);
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

    /// All valid backdoor sets Z for X --> Y up to size `max_size`.
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

    // -------- proper backdoor graph --------
    fn can_reach_any_y(&self, ys: &[u32]) -> Vec<bool> {
        let mut r = self.bitset();
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
    fn drop_first_edge(&self, xs: &[u32], reach_y: &[bool], row_u: u32, k: usize) -> bool {
        let c = self.core_ref();
        let v = c.col_index[k];
        if c.side[k] == 0 {
            xs.binary_search(&row_u).is_ok() && reach_y[v as usize]
        } else {
            xs.binary_search(&v).is_ok() && reach_y[row_u as usize]
        }
    }
    fn rebuild_filtered<F: FnMut(u32, usize) -> bool>(
        &self,
        mut drop: F,
    ) -> Result<CaugiGraph, String> {
        let c = self.core_ref();
        let n = self.n() as usize;
        let mut idx = vec![0u32; n + 1];
        for u in 0..self.n() {
            let mut keep = 0u32;
            for k in c.row_range(u) {
                if !drop(u, k) {
                    keep += 1;
                }
            }
            idx[u as usize + 1] = idx[u as usize] + keep;
        }
        let nnz = idx[n] as usize;
        let mut col = vec![0u32; nnz];
        let mut ety = vec![0u8; nnz];
        let mut side = vec![0u8; nnz];
        let mut cur = idx[..n].to_vec();
        for u in 0..self.n() {
            for k in c.row_range(u) {
                if drop(u, k) {
                    continue;
                }
                let p = cur[u as usize] as usize;
                col[p] = c.col_index[k];
                ety[p] = c.etype[k];
                side[p] = c.side[k];
                cur[u as usize] += 1;
            }
        }
        CaugiGraph::from_csr(idx, col, ety, side, c.simple, c.registry.clone())
    }
    /// Remove first edge of each proper causal path from any x∈xs to any y∈ys.
    pub fn proper_backdoor_graph(&self, xs: &[u32], ys: &[u32]) -> Result<Self, String> {
        let reach = self.can_reach_any_y(ys);
        let core = self.rebuild_filtered(|u, k| self.drop_first_edge(xs, &reach, u, k))?;
        Dag::new(Arc::new(core))
    }
    /// Valid Z for Xs→Ys in proper backdoor graph.
    pub fn is_valid_adjustment_set(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> bool {
        self.proper_backdoor_graph(xs, ys)
            .map(|g| g.is_d_separated(xs, ys, z))
            .unwrap_or(false)
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
    fn dag_is_d_separated_basic_patterns() {
        // Chain: 0->1->2. 0 ⟂̸ 2 | ∅ ; but 0 ⟂ 2 | {1}.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        assert_eq!(g.is_d_separated(&[0], &[2], &[]), false);
        assert_eq!(g.is_d_separated(&[0], &[2], &[1]), true);
    }

    #[test]
    fn dag_is_d_separated_collider_activation() {
        // Collider: 0->2<-1. 0 ⟂ 1 | ∅ ; but 0 ⟂̸ 1 | {2}.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        assert_eq!(g.is_d_separated(&[0], &[1], &[]), true);
        assert_eq!(g.is_d_separated(&[0], &[1], &[2]), false);
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
    fn dag_is_d_separated_empty_x_or_y_trivially_true() {
        // With empty X or Y, should return true by definition.
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        let g = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        assert!(g.is_d_separated(&[], &[1], &[]));
        assert!(g.is_d_separated(&[0], &[], &[]));
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
        assert!(!pb.is_d_separated(&[1], &[2], &[])); // A and Y still d-connected via L
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
}
