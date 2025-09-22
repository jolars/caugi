// src/graph/alg.rs
// SPDX-License-Identifier: MIT
use crate::graph::CaugiGraph;
use crate::edges::EdgeClass;

/// Returns true iff the directed part (→) of the graph is acyclic.
/// Ignores undirected/partial/bidirected edges, so it works for DAG, PDAG, CPDAG.
pub fn directed_part_is_acyclic(core: &CaugiGraph) -> bool {
    let n = core.n() as usize;
    let mut indeg = vec![0usize; n];

    // indegree: count incoming directed halves (side==1)
    for i in 0..n {
        for k in core.row_range(i as u32) {
            let spec = &core.registry.specs[core.etype[k] as usize];
            if matches!(spec.class, EdgeClass::Directed) && core.side[k] == 1 {
                indeg[i] += 1;
            }
        }
    }

    // Kahn
    let mut stack: Vec<usize> = (0..n).filter(|&i| indeg[i] == 0).collect();
    if stack.is_empty() { return false; }
    let mut seen = stack.len();

    while let Some(u) = stack.pop() {
        for k in core.row_range(u as u32) {
            let spec = &core.registry.specs[core.etype[k] as usize];
            if matches!(spec.class, EdgeClass::Directed) && core.side[k] == 0 {
                let v = core.col_index[k] as usize; // u -> v
                indeg[v] -= 1;
                if indeg[v] == 0 { stack.push(v); seen += 1; }
            }
        }
    }
    seen == n
}
