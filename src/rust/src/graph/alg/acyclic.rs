// SPDX-License-Identifier: MIT
//! Acyclicity testing for graphs.

use super::topo_sort::topological_sort;
use crate::graph::CaugiGraph;

/// Returns true iff the directed part of the graph is acyclic.
/// Ignores undirected/partial/bidirected edges.
///
/// This is implemented by running topological sort and checking if all nodes
/// were processed. If the directed part contains a cycle, topological sort
/// will not be able to process all nodes.
pub fn directed_part_is_acyclic(core: &CaugiGraph) -> bool {
    let n = core.n() as usize;
    // Empty graphs are trivially acyclic
    if n == 0 {
        return true;
    }
    topological_sort(core).len() == n
}

#[cfg(test)]
mod tests {
    use super::directed_part_is_acyclic;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn empty_stack_when_every_node_has_incoming_directed() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let code = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new(2, false, &reg);
        b.add_edge(0, 1, code).unwrap();
        b.add_edge(1, 0, code).unwrap();
        let g = b.finalize().unwrap();

        assert!(!directed_part_is_acyclic(&g));
    }

    #[test]
    fn directed_part_is_acyclic_ignores_undirected_and_bidirected() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let und = reg.code_of("---").unwrap();
        let bi = reg.code_of("<->").unwrap();

        // Directed part is a DAG: 0 -> 1 -> 2
        let mut b = GraphBuilder::new(3, false, &reg);
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        // Add an undirected 2-cycle and a bidirected edge; should be ignored by acyclicity
        b.add_edge(0, 2, und).unwrap();
        b.add_edge(2, 0, bi).unwrap();
        let g = b.finalize().unwrap();

        assert!(directed_part_is_acyclic(&g));
    }
}
