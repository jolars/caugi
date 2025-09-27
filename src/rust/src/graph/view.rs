// SPDX-License-Identifier: MIT
//! Unified graph view with dispatch to class-specific APIs.

use super::CaugiGraph;
use super::dag::Dag;
use super::pdag::Pdag;
use std::sync::Arc;

impl GraphView {
    pub fn core(&self) -> &CaugiGraph {
        match self {
            GraphView::Dag(d) => d.core_ref(),
            GraphView::Pdag(p) => p.core_ref(),
            GraphView::Raw(c) => c,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GraphView {
    Dag(Arc<Dag>),
    Pdag(Arc<Pdag>),
    Raw(Arc<CaugiGraph>),
    // ADMG, MAG, PAG to be added
}

pub trait GraphApi {
    fn parents_of(&self, _i: u32) -> Result<&[u32], String> {
        Err("parents_of not implemented for this class".into())
    }
    fn children_of(&self, _i: u32) -> Result<&[u32], String> {
        Err("children_of not implemented for this class".into())
    }
    fn undirected_of(&self, _i: u32) -> Result<&[u32], String> {
        Err("undirected_of not implemented for this class".into())
    }
    fn neighbors_of(&self, _i: u32) -> Result<&[u32], String> {
        Err("neighbors_of not implemented for this class".into())
    }
    fn n(&self) -> u32;
}

impl GraphApi for GraphView {
    fn parents_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.parents_of(i)),
            GraphView::Pdag(g) => Ok(g.parents_of(i)),
            GraphView::Raw(_) => Err("parents_of not implemented for UNKNOWN class".into()),
        }
    }
    fn children_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.children_of(i)),
            GraphView::Pdag(g) => Ok(g.children_of(i)),
            GraphView::Raw(_) => Err("children_of not implemented for UNKNOWN class".into()),
        }
    }
    fn undirected_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(_) => Err("undirected_of not defined for Dag".into()),
            GraphView::Pdag(g) => Ok(g.undirected_of(i)),
            GraphView::Raw(_) => Err("undirected_of not implemented for UNKNOWN class".into()),
        }
    }
    fn neighbors_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.neighbors_of(i)),
            GraphView::Pdag(g) => Ok(g.neighbors_of(i)),
            GraphView::Raw(_) => Err("neighbors_of not implemented for UNKNOWN class".into()),
        }
    }

    fn n(&self) -> u32 {
        match self {
            GraphView::Dag(g) => g.n(),
            GraphView::Pdag(g) => g.n(),
            GraphView::Raw(core) => core.n(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn graphview_dispatch_dag_pdag_raw() {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let cdir = r.code_of("-->").unwrap();
        let cund = r.code_of("---").unwrap();

        // DAG core: 0->1, 0->2
        let mut bd = GraphBuilder::new_with_registry(3, true, &r);
        bd.add_edge(0, 1, cdir).unwrap();
        bd.add_edge(0, 2, cdir).unwrap();
        let dag_core = Arc::new(bd.finalize().unwrap());
        let dag = Arc::new(Dag::new(dag_core.clone()).unwrap());
        let v_dag = GraphView::Dag(dag);
        assert_eq!(v_dag.n(), 3);
        assert_eq!(v_dag.parents_of(1).unwrap(), &[0]);
        assert_eq!(v_dag.children_of(0).unwrap(), &[1, 2]);
        assert_eq!(v_dag.neighbors_of(0).unwrap(), &[1, 2]);
        let e = v_dag.undirected_of(0).unwrap_err();
        assert_eq!(e, "undirected_of not defined for Dag");
        let v_dag_core = v_dag.core();
        assert_eq!(v_dag_core.n(), 3);

        // PDAG core: 0->1, 1---2
        let mut bp = GraphBuilder::new_with_registry(3, true, &r);
        bp.add_edge(0, 1, cdir).unwrap();
        bp.add_edge(1, 2, cund).unwrap();
        let pdag_core = Arc::new(bp.finalize().unwrap());
        let pdag = Arc::new(Pdag::new(pdag_core.clone()).unwrap());
        let v_pdag = GraphView::Pdag(pdag);
        assert_eq!(v_pdag.n(), 3);
        assert_eq!(v_pdag.parents_of(1).unwrap(), &[0]);
        assert_eq!(v_pdag.children_of(0).unwrap(), &[1]);
        assert_eq!(v_pdag.undirected_of(1).unwrap(), &[2]);
        assert_eq!(v_pdag.neighbors_of(1).unwrap(), &[0, 2]);
        let v_pdag_core = v_pdag.core();
        assert_eq!(v_pdag_core.n(), 3);

        // Raw view uses UNKNOWN class fallbacks
        let v_raw = GraphView::Raw(pdag_core);
        assert_eq!(v_raw.n(), 3);
        let e1 = v_raw.parents_of(0).unwrap_err();
        let e2 = v_raw.children_of(0).unwrap_err();
        let e3 = v_raw.undirected_of(0).unwrap_err();
        assert_eq!(e1, "parents_of not implemented for UNKNOWN class");
        assert_eq!(e2, "children_of not implemented for UNKNOWN class");
        assert_eq!(e3, "undirected_of not implemented for UNKNOWN class");
        assert_eq!(
            v_raw.neighbors_of(0).unwrap_err(),
            "neighbors_of not implemented for UNKNOWN class"
        );

        let v_raw_core = v_raw.core();
        assert_eq!(v_raw_core.n(), 3);
    }

    #[test]
    fn graphapi_default_methods_are_hit() {
        struct Dummy(u32);
        impl GraphApi for Dummy {
            fn n(&self) -> u32 {
                self.0
            }
        }

        let d = Dummy(5);
        assert_eq!(d.n(), 5);
        assert_eq!(
            d.parents_of(0).unwrap_err(),
            "parents_of not implemented for this class"
        );
        assert_eq!(
            d.children_of(0).unwrap_err(),
            "children_of not implemented for this class"
        );
        assert_eq!(
            d.undirected_of(0).unwrap_err(),
            "undirected_of not implemented for this class"
        );
        assert_eq!(
            d.neighbors_of(0).unwrap_err(),
            "neighbors_of not implemented for this class"
        );
    }
}
