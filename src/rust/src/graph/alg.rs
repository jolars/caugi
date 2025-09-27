// SPDX-License-Identifier: MIT
//! Graph algorithms.

use crate::edges::EdgeClass;
use crate::graph::CaugiGraph;
use crate::graph::graph_type::GraphType;

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

pub fn check_edge_classes(graph_type: &GraphType, core: &CaugiGraph) -> Result<(), String> {
    let (name, allowed, _) = graph_type.spec();
    for i in 0..core.n() {
        for k in core.row_range(i) {
            let cls = core.registry.specs[core.etype[k] as usize].class;
            if !allowed.iter().any(|&a| a == cls) {
                return Err(format!("Graph of type {} cannot contain edges of type {:?}", name, cls));
            }
        }
    }
    Ok(())
}

pub fn validate_graph_type(graph_type: &GraphType, core: &CaugiGraph) -> Result<(), String> {
    let (name, _, require_acyclic) = graph_type.spec();
    // Check edge classes
    check_edge_classes(&graph_type, core)?;

    // Check acyclicity if required
    if require_acyclic {
        if !directed_part_is_acyclic(core) {
            return Err(format!("Graph of type {} cannot contain directed cycle", name));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{directed_part_is_acyclic, check_edge_classes, validate_graph_type};
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;
    use crate::graph::graph_type::GraphType;

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

    #[test]
    fn directed_part_is_acyclic_ignores_undirected_and_bidirected() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let und = reg.code_of("---").unwrap();
        let bi  = reg.code_of("<->").unwrap();

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

    #[test]
    fn check_edge_classes_dag_rejects_undirected_and_bidirected_and_partial() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let und = reg.code_of("---").unwrap();
        let bi  = reg.code_of("<->").unwrap();
        let par = reg.code_of("o->").unwrap();

        // Valid DAG: OK
        let mut b1 = GraphBuilder::new(2, false, &reg);
        b1.add_edge(0, 1, dir).unwrap();
        let g1 = b1.finalize().unwrap();
        assert!(check_edge_classes(&GraphType::Dag, &g1).is_ok());

        // Undirected in DAG: Err
        let mut b2 = GraphBuilder::new(2, false, &reg);
        b2.add_edge(0, 1, und).unwrap();
        let g2 = b2.finalize().unwrap();
        assert!(check_edge_classes(&GraphType::Dag, &g2).is_err());

        // Bidirected in DAG: Err
        let mut b3 = GraphBuilder::new(2, false, &reg);
        b3.add_edge(0, 1, bi).unwrap();
        let g3 = b3.finalize().unwrap();
        assert!(check_edge_classes(&GraphType::Dag, &g3).is_err());

        // Partial in DAG: Err
        let mut b4 = GraphBuilder::new(2, false, &reg);
        b4.add_edge(0, 1, par).unwrap();
        let g4 = b4.finalize().unwrap();
        assert!(check_edge_classes(&GraphType::Dag, &g4).is_err());
    }

    #[test]
    fn check_edge_classes_pdag_allows_directed_and_undirected_but_rejects_bidirected_and_partial() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let und = reg.code_of("---").unwrap();
        let bi  = reg.code_of("<->").unwrap();
        let par = reg.code_of("o->").unwrap();

        // Directed + undirected: OK
        let mut b1 = GraphBuilder::new(3, false, &reg);
        b1.add_edge(0, 1, dir).unwrap();
        b1.add_edge(1, 2, und).unwrap();
        let g1 = b1.finalize().unwrap();
        assert!(check_edge_classes(&GraphType::Pdag, &g1).is_ok());

        // Bidirected present: Err
        let mut b2 = GraphBuilder::new(2, false, &reg);
        b2.add_edge(0, 1, bi).unwrap();
        let g2 = b2.finalize().unwrap();
        assert!(check_edge_classes(&GraphType::Pdag, &g2).is_err());

        // Partial present: Err
        let mut b3 = GraphBuilder::new(2, false, &reg);
        b3.add_edge(0, 1, par).unwrap();
        let g3 = b3.finalize().unwrap();
        assert!(check_edge_classes(&GraphType::Pdag, &g3).is_err());
    }

    #[test]
    fn validate_graph_type_dag_catches_directed_cycle() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new(3, false, &reg);
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        b.add_edge(2, 0, dir).unwrap(); // cycle
        let g = b.finalize().unwrap();

        let err = validate_graph_type(&GraphType::Dag, &g).unwrap_err();
        assert!(err.contains("directed cycle"));
    }

    #[test]
    fn validate_graph_type_dag_ok_when_acyclic() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new(3, false, &reg);
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();
        let g = b.finalize().unwrap();

        assert!(validate_graph_type(&GraphType::Dag, &g).is_ok());
    }
}

