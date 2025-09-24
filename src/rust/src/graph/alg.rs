// SPDX-License-Identifier: MIT
//! Graph algorithms.

use crate::edges::EdgeClass;
use crate::graph::CaugiGraph;

/// Returns true iff the directed part of the graph is acyclic.
/// Ignores undirected/partial/bidirected edges
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
    if stack.is_empty() {
        return false;
    }
    let mut seen = stack.len();

    while let Some(u) = stack.pop() {
        for k in core.row_range(u as u32) {
            let spec = &core.registry.specs[core.etype[k] as usize];
            if matches!(spec.class, EdgeClass::Directed) && core.side[k] == 0 {
                let v = core.col_index[k] as usize; // u -> v
                indeg[v] -= 1;
                if indeg[v] == 0 {
                    stack.push(v);
                    seen += 1;
                }
            }
        }
    }
    seen == n
}

#[cfg(test)]
mod tests {
    use super::directed_part_is_acyclic;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn empty_stack_when_every_node_has_incoming_directed() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let code = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new(2, false, &reg);
        b.add_edge(0, 1, code).unwrap();
        b.add_edge(1, 0, code).unwrap();
        let g = b.finalize().unwrap();

        assert!(!directed_part_is_acyclic(&g));
    }
}

