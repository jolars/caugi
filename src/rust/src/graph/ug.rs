// SPDX-License-Identifier: MIT
//! UG (Undirected Graph) wrapper with O(1) slice queries via packed neighborhoods.

use super::CaugiGraph;
use crate::edges::EdgeClass;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Ug {
    core: Arc<CaugiGraph>,
    /// len = n+1
    node_edge_ranges: Arc<[usize]>,
    /// len = n; (neighbors)
    // node_deg: Arc<[u32]>,
    /// packed neighbors
    neighborhoods: Arc<[u32]>,
}

impl Ug {
    pub fn new(core: Arc<CaugiGraph>) -> Result<Self, String> {
        let n = core.n() as usize;

        // Count neighbors per node and validate edges are undirected
        let mut deg: Vec<u32> = vec![0; n];
        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Undirected => deg[i] += 1,
                    // Reject any non-undirected edges
                    _ => {
                        return Err("UG can only contain undirected edges".into());
                    }
                }
            }
        }

        let mut node_edge_ranges = Vec::with_capacity(n + 1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let last = *node_edge_ranges.last().unwrap();
            node_edge_ranges.push(last + deg[i] as usize);
        }

        let total = *node_edge_ranges.last().unwrap();
        let mut neigh = vec![0u32; total];

        // Fill neighbor arrays
        let mut base: Vec<usize> = vec![0; n];
        for i in 0..n {
            base[i] = node_edge_ranges[i];
        }
        let mut cur = base.clone();

        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                let p = cur[i];
                neigh[p] = core.col_index[k];
                cur[i] += 1;
            }
            // Sort neighbors for determinism
            let s = node_edge_ranges[i];
            let e = node_edge_ranges[i + 1];
            neigh[s..e].sort_unstable();
        }

        Ok(Self {
            core,
            node_edge_ranges: node_edge_ranges.into(),
            // node_deg: deg.into(),
            neighborhoods: neigh.into(),
        })
    }

    #[inline]
    pub fn n(&self) -> u32 {
        self.core.n()
    }

    #[inline]
    pub fn neighbors_of(&self, i: u32) -> &[u32] {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        &self.neighborhoods[s..e]
    }

    #[inline]
    pub fn markov_blanket_of(&self, i: u32) -> Vec<u32> {
        // In an undirected graph, the Markov blanket is just the neighbors
        self.neighbors_of(i).to_vec()
    }

    #[inline]
    pub fn exogenous_nodes(&self) -> Vec<u32> {
        // In an undirected graph, nodes with no edges are exogenous
        (0..self.n())
            .filter(|&i| self.neighbors_of(i).is_empty())
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
    fn ug_basic_construction() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cund).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let g = Ug::new(core).expect("UG construction failed");
        assert_eq!(g.n(), 3);
    }

    #[test]
    fn ug_neighbors() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, cund).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        b.add_edge(2, 3, cund).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let g = Ug::new(core).expect("UG construction failed");

        assert_eq!(g.neighbors_of(0), vec![1]);
        assert_eq!(g.neighbors_of(1), vec![0, 2]);
        assert_eq!(g.neighbors_of(2), vec![1, 3]);
        assert_eq!(g.neighbors_of(3), vec![2]);
    }

    #[test]
    fn ug_rejects_directed_edges() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let r = Ug::new(core);
        assert!(r.is_err());
        assert!(r.unwrap_err().contains("undirected"));
    }

    #[test]
    fn ug_rejects_bidirected_edges() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cbid = reg.code_of("<->").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, cbid).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let r = Ug::new(core);
        assert!(r.is_err());
    }

    #[test]
    fn ug_markov_blanket() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, cund).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        b.add_edge(1, 3, cund).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let g = Ug::new(core).expect("UG construction failed");

        // Markov blanket of node 1 should be all its neighbors
        assert_eq!(g.markov_blanket_of(1), vec![0, 2, 3]);
        // Node 0 has only one neighbor
        assert_eq!(g.markov_blanket_of(0), vec![1]);
    }

    #[test]
    fn ug_exogenous_nodes() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        b.add_edge(0, 1, cund).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        // nodes 3 and 4 are isolated
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let g = Ug::new(core).expect("UG construction failed");

        assert_eq!(g.exogenous_nodes(), vec![3, 4]);
    }

    #[test]
    fn ug_core_ref() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, cund).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let g = Ug::new(core).expect("UG construction failed");

        let core = g.core_ref();
        assert_eq!(core.n(), 2);
    }
}
