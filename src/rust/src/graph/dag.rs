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
        // First pass: count per-row (pa,ch)
        let mut deg: Vec<(u32, u32)> = vec![(0, 0); n];
        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
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
        // Prefix sums for ranges
        let mut node_edge_ranges = Vec::with_capacity(n + 1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let (pa, ch) = deg[i];
            let last = *node_edge_ranges.last().unwrap();
            node_edge_ranges.push(last + (pa + ch) as usize);
        }
        let mut neigh = vec![0u32; *node_edge_ranges.last().unwrap()];
        // Per-row cursors
        let mut cur = vec![0usize; n];
        for i in 0..n {
            cur[i] = node_edge_ranges[i];
        }
        // Parents first segment, children second; compute child base once
        let mut child_base: Vec<usize> = vec![0; n];
        for i in 0..n {
            let (pa, _) = deg[i];
            child_base[i] = node_edge_ranges[i] + pa as usize;
        }
        let mut child_cur = child_base.clone();

        // Second pass: scatter into packed layout
        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                if core.side[k] == 1 {
                    let p = cur[i];
                    neigh[p] = core.col_index[k];
                    cur[i] += 1;
                } else {
                    let p = child_cur[i];
                    neigh[p] = core.col_index[k];
                    child_cur[i] += 1;
                }
            }
            // determinism
            let start = node_edge_ranges[i];
            let mid = child_base[i];
            let end = node_edge_ranges[i + 1];
            neigh[start..mid].sort_unstable();
            neigh[mid..end].sort_unstable();
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
}
