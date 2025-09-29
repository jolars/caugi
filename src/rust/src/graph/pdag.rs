// SPDX-License-Identifier: MIT
//! Pdag wrapper with O(1) slice queries via packed neighborhoods.

use super::CaugiGraph;
use crate::edges::EdgeClass;
use crate::graph::alg::directed_part_is_acyclic;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Pdag {
    core: Arc<CaugiGraph>,
    /// len = n+1
    node_edge_ranges: Arc<[usize]>,
    /// len = n; (parents, undirected, children)
    node_deg: Arc<[(u32, u32, u32)]>,
    /// packed as [parents | undirected | children]
    neighbourhoods: Arc<[u32]>,
}

impl Pdag {
    pub fn new(core: Arc<CaugiGraph>) -> Result<Self, String> {
        let n = core.n() as usize;
        if !directed_part_is_acyclic(&core) {
            return Err("PDAG contains a directed cycle".into());
        }
        let mut deg: Vec<(u32, u32, u32)> = vec![(0, 0, 0); n];
        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Directed => {
                        if core.side[k] == 1 {
                            deg[i].0 += 1
                        } else {
                            deg[i].2 += 1
                        }
                    }
                    EdgeClass::Undirected => deg[i].1 += 1,
                    // Throw error on partial/bidirected edges
                    _ => {
                        return Err("Pdag cannot contain partial/bidirected edges".into());
                    }
                }
            }
        }
        let mut node_edge_ranges = Vec::with_capacity(n + 1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let (pa, u, ch) = deg[i];
            let last = *node_edge_ranges.last().unwrap();
            node_edge_ranges.push(last + (pa + u + ch) as usize);
        }
        let total = *node_edge_ranges.last().unwrap();
        let mut neigh = vec![0u32; total];

        // bucket bases
        let mut parent_base: Vec<usize> = vec![0; n];
        let mut und_base: Vec<usize> = vec![0; n];
        let mut child_base: Vec<usize> = vec![0; n];
        for i in 0..n {
            let start = node_edge_ranges[i];
            let (pa, u, _) = deg[i];
            parent_base[i] = start;
            und_base[i] = start + pa as usize;
            child_base[i] = und_base[i] + u as usize;
        }
        let mut pcur = parent_base.clone();
        let mut ucur = und_base.clone();
        let mut ccur = child_base.clone();

        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
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
                    EdgeClass::Undirected => {
                        let p = ucur[i];
                        neigh[p] = core.col_index[k];
                        ucur[i] += 1;
                    }
                    _ => {
                        // Is only here to satisfy exhaustiveness. It's unreachable
                        unreachable!("Should have errored on partial/bidirected edges earlier");
                    }
                }
            }
            // determinism
            let s = node_edge_ranges[i];
            let pm = und_base[i];
            let um = child_base[i];
            let e = node_edge_ranges[i + 1];
            neigh[s..pm].sort_unstable();
            neigh[pm..um].sort_unstable();
            neigh[um..e].sort_unstable();
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
    fn bounds(&self, i: u32) -> (usize, usize, usize, usize) {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        let (pa, u, ch) = self.node_deg[i];
        let pm = s + pa as usize;
        let um = pm + u as usize;
        let cs = e - ch as usize;
        (s, pm, um, cs)
    }

    #[inline]
    pub fn parents_of(&self, i: u32) -> &[u32] {
        let (s, pm, _, _) = self.bounds(i);
        &self.neighbourhoods[s..pm]
    }
    #[inline]
    pub fn children_of(&self, i: u32) -> &[u32] {
        let (_, _, _, cs) = self.bounds(i);
        let e = self.node_edge_ranges[i as usize + 1];
        &self.neighbourhoods[cs..e]
    }
    #[inline]
    pub fn undirected_of(&self, i: u32) -> &[u32] {
        let (_, pm, um, _) = self.bounds(i);
        &self.neighbourhoods[pm..um]
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
            if seen[ui] { continue; }
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
            if seen[ui] { continue; }
            seen[ui] = true;
            out.push(u);
            stack.extend_from_slice(self.children_of(u));
        }
        out.sort_unstable();
        out
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
    fn pdag_relations() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let g = Pdag::new(core).expect("Pdag construction failed");
        assert_eq!(g.parents_of(1), vec![0]);
        assert_eq!(g.children_of(0), vec![1]);
        let mut u = g.undirected_of(1).to_vec();
        u.sort_unstable();
        assert_eq!(u, vec![2]);
        assert_eq!(g.n(), 3);
        assert_eq!(g.neighbors_of(0), vec![1]);
        assert_eq!(g.neighbors_of(1), vec![0, 2]);
        assert_eq!(g.neighbors_of(2), vec![1]);

        // get core
        let core = g.core_ref();
        assert_eq!(core.n(), 3);
    }

    #[test]
    fn pdag_cycle_error() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cdir).unwrap();
        b.add_edge(2, 0, cdir).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let r = Pdag::new(core);
        assert!(r.is_err());
    }
    
    #[test]
    fn pdag_anc_desc_directed_only() {
        // 0 -> 1 -- 2
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap(); let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0,1,cdir).unwrap(); b.add_edge(1,2,cund).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.ancestors_of(1), vec![0]);
        assert_eq!(g.descendants_of(0), vec![1]);
        assert!(g.ancestors_of(2).is_empty());
        assert!(g.descendants_of(2).is_empty());
    }

    #[test]
    fn pdag_partial_edge_error() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cpar = reg.code_of("o->").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, cpar).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let r = Pdag::new(core);
        assert!(r.is_err());
    }
}
