// src/graph/dag.rs
// SPDX-License-Identifier: MIT
//! DAG wrapper with O(1) slice queries via packed neighborhoods.

use std::sync::Arc;
use super::CaugiGraph;
use crate::edges::EdgeClass;

#[derive(Debug, Clone)]
pub struct Dag {
    core: Arc<CaugiGraph>,
    /// len = n+1; row i is node_edge_ranges[i]..node_edge_ranges[i+1]
    node_edge_ranges: Arc<[usize]>,
    /// len = n; (parents, children)
    node_deg: Arc<[(u32, u32)]>,
    /// packed as [parents | children] per row, endpoints as node ids
    neighbourhoods: Arc<[u32]>,
}

impl Dag {
    pub fn new(core: Arc<CaugiGraph>) -> Self {
        let n = core.n() as usize;
        // First pass: count per-row (pa,ch)
        let mut deg: Vec<(u32,u32)> = vec![(0,0); n];
        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                let spec = &core.registry.specs[core.etype[k] as usize];
                if !matches!(spec.class, EdgeClass::Directed) { continue; }
                if core.side[k] == 1 { deg[i].0 += 1 } else { deg[i].1 += 1 }
            }
        }
        // Prefix sums for ranges
        let mut node_edge_ranges = Vec::with_capacity(n+1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let (pa,ch) = deg[i];
            let last = *node_edge_ranges.last().unwrap();
            node_edge_ranges.push(last + (pa+ch) as usize);
        }
        let mut neigh = vec![0u32; *node_edge_ranges.last().unwrap()];
        // Per-row cursors
        let mut cur = vec![0usize; n];
        for i in 0..n { cur[i] = node_edge_ranges[i]; }
        // Parents first segment, children second; compute child base once
        let mut child_base: Vec<usize> = vec![0; n];
        for i in 0..n {
            let (pa,_) = deg[i];
            child_base[i] = node_edge_ranges[i] + pa as usize;
        }
        let mut child_cur = child_base.clone();

        // Second pass: scatter into packed layout
        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                let spec = &core.registry.specs[core.etype[k] as usize];
                if !matches!(spec.class, EdgeClass::Directed) { continue; }
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
            let mid   = child_base[i];
            let end   = node_edge_ranges[i+1];
            neigh[start..mid].sort_unstable();
            neigh[mid..end].sort_unstable();
        }

        Self {
            core,
            node_edge_ranges: node_edge_ranges.into(),
            node_deg: deg.into(),
            neighbourhoods: neigh.into(),
        }
    }

    #[inline] pub fn n(&self) -> u32 { self.core.n() }
    #[inline] fn row_bounds(&self, i: u32) -> (usize,usize,usize) {
        let i = i as usize;
        let start = self.node_edge_ranges[i];
        let end   = self.node_edge_ranges[i+1];
        let (pa,ch) = self.node_deg[i];
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
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn dag_parents_children() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0,1,cdir).unwrap();
        b.add_edge(0,2,cdir).unwrap();
        b.add_edge(3,0,cdir).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let dag = Dag::new(core);
        assert_eq!(dag.children_of(0), vec![1,2]);
        assert_eq!(dag.parents_of(0), vec![3]);
        assert_eq!(dag.parents_of(1), vec![0]);
        assert!(dag.children_of(1).is_empty());
        assert_eq!(dag.children_of(3), vec![0]);
    }
}