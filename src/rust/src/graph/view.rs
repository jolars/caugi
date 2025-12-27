use super::admg::Admg;
use super::dag::Dag;
use super::pdag::Pdag;
use super::ug::Ug;
use super::CaugiGraph;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum GraphView {
    Dag(Arc<Dag>),
    Pdag(Arc<Pdag>),
    Ug(Arc<Ug>),
    Admg(Arc<Admg>),
    Raw(Arc<CaugiGraph>),
}

impl GraphView {
    #[inline]
    pub fn core(&self) -> &CaugiGraph {
        match self {
            GraphView::Dag(d) => d.core_ref(),
            GraphView::Pdag(p) => p.core_ref(),
            GraphView::Ug(u) => u.core_ref(),
            GraphView::Admg(a) => a.core_ref(),
            GraphView::Raw(c) => c,
        }
    }

    #[inline]
    pub fn n(&self) -> u32 {
        match self {
            GraphView::Dag(g) => g.n(),
            GraphView::Pdag(g) => g.n(),
            GraphView::Ug(g) => g.n(),
            GraphView::Admg(g) => g.n(),
            GraphView::Raw(core) => core.n(),
        }
    }

    // ---- queries ----
    pub fn parents_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.parents_of(i)),
            GraphView::Pdag(g) => Ok(g.parents_of(i)),
            GraphView::Admg(g) => Ok(g.parents_of(i)),
            GraphView::Ug(_) => Err("parents_of not defined for UG".into()),
            GraphView::Raw(_) => Err("parents_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn children_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.children_of(i)),
            GraphView::Pdag(g) => Ok(g.children_of(i)),
            GraphView::Admg(g) => Ok(g.children_of(i)),
            GraphView::Ug(_) => Err("children_of not defined for UG".into()),
            GraphView::Raw(_) => Err("children_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn undirected_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(_) => Err("undirected_of not defined for Dag".into()),
            GraphView::Admg(_) => Err("undirected_of not defined for ADMG".into()),
            GraphView::Pdag(g) => Ok(g.undirected_of(i)),
            GraphView::Ug(g) => Ok(g.neighbors_of(i)),
            GraphView::Raw(_) => Err("undirected_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn neighbors_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.neighbors_of(i)),
            GraphView::Pdag(g) => Ok(g.neighbors_of(i)),
            GraphView::Ug(g) => Ok(g.neighbors_of(i)),
            GraphView::Admg(g) => Ok(g.neighbors_of(i)),
            GraphView::Raw(_) => Err("neighbors_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn ancestors_of(&self, i: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.ancestors_of(i)),
            GraphView::Pdag(g) => Ok(g.ancestors_of(i)),
            GraphView::Admg(g) => Ok(g.ancestors_of(i)),
            GraphView::Ug(_) => Err("ancestors_of not defined for UG".into()),
            GraphView::Raw(_) => Err("ancestors_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn descendants_of(&self, i: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.descendants_of(i)),
            GraphView::Pdag(g) => Ok(g.descendants_of(i)),
            GraphView::Admg(g) => Ok(g.descendants_of(i)),
            GraphView::Ug(_) => Err("descendants_of not defined for UG".into()),
            GraphView::Raw(_) => Err("descendants_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn markov_blanket_of(&self, i: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.markov_blanket_of(i)),
            GraphView::Pdag(g) => Ok(g.markov_blanket_of(i)),
            GraphView::Ug(g) => Ok(g.markov_blanket_of(i)),
            GraphView::Admg(g) => Ok(g.markov_blanket_of(i)),
            GraphView::Raw(_) => Err("markov_blanket_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn exogenous_nodes(&self, undirected_as_parents: bool) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.exogenous_nodes()),
            GraphView::Pdag(g) => Ok(g.exogenous_nodes(undirected_as_parents)),
            GraphView::Ug(g) => Ok(g.exogenous_nodes()),
            GraphView::Admg(g) => Ok(g.exogenous_nodes()),
            GraphView::Raw(_) => Err("exogenous_nodes not implemented for UNKNOWN class".into()),
        }
    }

    /// Returns a topological ordering of the nodes.
    ///
    /// Only defined for DAGs. Returns all nodes in an order such that
    /// for every directed edge u -> v, u appears before v.
    pub fn topological_sort(&self) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.topological_sort()),
            GraphView::Admg(_) => Err("topological_sort is only defined for DAGs".into()),
            GraphView::Pdag(_) => Err("topological_sort is only defined for DAGs".into()),
            GraphView::Ug(_) => Err("topological_sort is only defined for DAGs".into()),
            GraphView::Raw(_) => Err("topological_sort is only defined for DAGs".into()),
        }
    }

    // ---- ADMG-specific methods ----
    pub fn spouses_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Admg(g) => Ok(g.spouses_of(i)),
            _ => Err("spouses_of is only defined for ADMGs".into()),
        }
    }

    pub fn districts(&self) -> Result<Vec<Vec<u32>>, String> {
        match self {
            GraphView::Admg(g) => Ok(g.districts()),
            _ => Err("districts is only defined for ADMGs".into()),
        }
    }

    pub fn district_of(&self, i: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Admg(g) => Ok(g.district_of(i)),
            _ => Err("district_of is only defined for ADMGs".into()),
        }
    }

    pub fn m_separated(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> Result<bool, String> {
        match self {
            GraphView::Admg(g) => Ok(g.m_separated(xs, ys, z)),
            GraphView::Dag(d) => Ok(d.d_separated(xs, ys, z)), // d-sep is m-sep for DAGs
            _ => Err("m_separated is only defined for ADMGs and DAGs".into()),
        }
    }

    // ---- DAG-only methods ----
    pub fn d_separated(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> Result<bool, String> {
        match self {
            GraphView::Dag(d) => Ok(d.d_separated(xs, ys, z)),
            _ => Err("d_separated is only defined for DAGs".into()),
        }
    }
    pub fn adjustment_set_parents(&self, xs: &[u32], ys: &[u32]) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(d) => Ok(d.adjustment_set_parents(xs, ys)),
            _ => Err("adjustment_set_parents is only defined for DAGs".into()),
        }
    }
    pub fn adjustment_set_backdoor(&self, xs: &[u32], ys: &[u32]) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(d) => Ok(d.adjustment_set_backdoor(xs, ys)),
            _ => Err("adjustment_set_backdoor is only defined for DAGs".into()),
        }
    }
    pub fn adjustment_set_optimal(&self, x: u32, y: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(d) => Ok(d.adjustment_set_optimal(x, y)),
            _ => Err("adjustment_set_optimal is only defined for DAGs".into()),
        }
    }
    pub fn is_valid_backdoor_set(&self, x: u32, y: u32, z: &[u32]) -> Result<bool, String> {
        match self {
            GraphView::Dag(d) => Ok(d.is_valid_backdoor_set(x, y, z)),
            _ => Err("is_valid_backdoor_set is only defined for DAGs".into()),
        }
    }
    pub fn all_backdoor_sets(
        &self,
        x: u32,
        y: u32,
        minimal: bool,
        max_size: u32,
    ) -> Result<Vec<Vec<u32>>, String> {
        match self {
            GraphView::Dag(d) => Ok(d.all_backdoor_sets(x, y, minimal, max_size)),
            _ => Err("all_backdoor_sets is only defined for DAGs".into()),
        }
    }

    // ---- ADMG adjustment methods ----
    pub fn is_valid_adjustment_set_admg(
        &self,
        xs: &[u32],
        ys: &[u32],
        z: &[u32],
    ) -> Result<bool, String> {
        match self {
            GraphView::Admg(g) => Ok(g.is_valid_adjustment_set(xs, ys, z)),
            _ => Err("is_valid_adjustment_set_admg is only defined for ADMGs".into()),
        }
    }

    pub fn all_adjustment_sets_admg(
        &self,
        xs: &[u32],
        ys: &[u32],
        minimal: bool,
        max_size: u32,
    ) -> Result<Vec<Vec<u32>>, String> {
        match self {
            GraphView::Admg(g) => Ok(g.all_adjustment_sets(xs, ys, minimal, max_size)),
            _ => Err("all_adjustment_sets_admg is only defined for ADMGs".into()),
        }
    }

    pub fn induced_subgraph(&self, keep: &[u32]) -> Result<GraphView, String> {
        let (core2, _new_to_old, _old_to_new) = self.core().induced_subgraph(keep)?;
        let gv = match self {
            GraphView::Dag(_) => {
                let d = super::dag::Dag::new(std::sync::Arc::new(core2))?;
                GraphView::Dag(std::sync::Arc::new(d))
            }
            GraphView::Pdag(_) => {
                let p = super::pdag::Pdag::new(std::sync::Arc::new(core2))?;
                GraphView::Pdag(std::sync::Arc::new(p))
            }
            GraphView::Ug(_) => {
                let u = super::ug::Ug::new(std::sync::Arc::new(core2))?;
                GraphView::Ug(std::sync::Arc::new(u))
            }
            GraphView::Admg(_) => {
                let a = super::admg::Admg::new(std::sync::Arc::new(core2))?;
                GraphView::Admg(std::sync::Arc::new(a))
            }
            GraphView::Raw(_) => GraphView::Raw(std::sync::Arc::new(core2)),
        };
        Ok(gv)
    }
}

impl GraphView {
    pub fn to_cpdag(&self) -> Result<GraphView, String> {
        match self {
            GraphView::Dag(d) => {
                let p = d.to_cpdag()?;
                Ok(GraphView::Pdag(std::sync::Arc::new(p)))
            }
            GraphView::Pdag(_) => Ok(self.clone()),
            _ => Err("to_cpdag is only defined for DAGs".into()),
        }
    }

    pub fn skeleton(&self) -> Result<GraphView, String> {
        match self {
            GraphView::Dag(d) => {
                let ug = d.skeleton()?;
                Ok(GraphView::Ug(Arc::new(ug)))
            }
            GraphView::Pdag(p) => {
                let ug = p.skeleton()?;
                Ok(GraphView::Ug(Arc::new(ug)))
            }
            _ => Err("skeleton is defined for DAGs and PDAGs only".into()),
        }
    }

    pub fn moralize(&self) -> Result<GraphView, String> {
        match self {
            GraphView::Dag(d) => {
                let ug = d.moralize()?;
                Ok(GraphView::Ug(Arc::new(ug)))
            }
            _ => Err("moralize is only defined for DAGs".into()),
        }
    }

    /// Project out latent variables from a DAG to produce an ADMG.
    ///
    /// For each pair of observed nodes (X, Y), adds a bidirected edge X <-> Y
    /// if they share a latent ancestor. Directed edges between observed nodes
    /// are preserved.
    ///
    /// # Arguments
    /// * `latents` - Slice of node indices to project out (0-indexed)
    ///
    /// # Returns
    /// An ADMG `GraphView` containing only the observed (non-latent) nodes.
    pub fn latent_project(&self, latents: &[u32]) -> Result<GraphView, String> {
        match self {
            GraphView::Dag(d) => {
                let admg = d.latent_project(latents)?;
                Ok(GraphView::Admg(Arc::new(admg)))
            }
            _ => Err("latent_project is only defined for DAGs".into()),
        }
    }

    /// Proper backdoor graph for Xs → Ys. Defined for DAG only.
    pub fn proper_backdoor_graph(&self, xs: &[u32], ys: &[u32]) -> Result<GraphView, String> {
        match self {
            GraphView::Dag(d) => {
                let core = d.proper_backdoor_core(xs, ys)?;
                let dag = super::dag::Dag::new(Arc::new(core))?;
                Ok(GraphView::Dag(Arc::new(dag)))
            }
            _ => Err("proper_backdoor_graph is only defined for DAGs".into()),
        }
    }

    /// Moral graph of the ancestral subgraph of seeds. Defined for DAG only.
    pub fn moral_of_ancestors(&self, seeds: &[u32]) -> Result<GraphView, String> {
        match self {
            GraphView::Dag(d) => {
                use crate::graph::alg::{bitset, csr, moral};
                let mask = bitset::ancestors_mask(seeds, |u| d.parents_of(u), d.n());
                let adj = moral::moral_adj(d.n(), |u| d.parents_of(u), &mask);
                let core = csr::build_ug_core_from_adj(d.core_ref(), &adj)?;
                let ug = super::ug::Ug::new(Arc::new(core))?;
                Ok(GraphView::Ug(Arc::new(ug)))
            }
            _ => Err("moral_of_ancestors is only defined for DAGs".into()),
        }
    }

    /// Ancestral reduction induced on seeds. Defined for DAG, PDAG, and ADMG.
    pub fn ancestral_reduction(&self, seeds: &[u32]) -> Result<GraphView, String> {
        match self {
            GraphView::Dag(d) => {
                use crate::graph::alg::bitset;
                let mask = bitset::ancestors_mask(seeds, |u| d.parents_of(u), d.n());
                let keep = bitset::collect_from_mask(&mask);
                self.induced_subgraph(&keep)
            }
            GraphView::Pdag(p) => {
                use crate::graph::alg::bitset;
                let mask = bitset::ancestors_mask(seeds, |u| p.parents_of(u), p.n());
                let keep = bitset::collect_from_mask(&mask);
                self.induced_subgraph(&keep)
            }
            GraphView::Admg(a) => {
                use crate::graph::alg::bitset;
                let mask = bitset::ancestors_mask(seeds, |u| a.parents_of(u), a.n());
                let keep = bitset::collect_from_mask(&mask);
                self.induced_subgraph(&keep)
            }
            _ => Err("ancestral_reduction is only defined for DAGs, PDAGs, and ADMGs".into()),
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
        assert_eq!(v_dag.descendants_of(0).unwrap(), vec![1, 2]);
        assert_eq!(v_dag.ancestors_of(2).unwrap(), vec![0]);
        let e = v_dag.undirected_of(0).unwrap_err();
        assert_eq!(e, "undirected_of not defined for Dag");
        let v_dag_core = v_dag.core();
        assert_eq!(v_dag_core.n(), 3);

        // PDAG core: 0->1, 1---2
        let mut bp = GraphBuilder::new_with_registry(4, true, &r);
        bp.add_edge(0, 1, cdir).unwrap();
        bp.add_edge(1, 2, cund).unwrap();
        bp.add_edge(1, 3, cdir).unwrap();
        let pdag_core = Arc::new(bp.finalize().unwrap());
        let pdag = Arc::new(Pdag::new(pdag_core.clone()).unwrap());
        let v_pdag = GraphView::Pdag(pdag);
        assert_eq!(v_pdag.n(), 4);
        assert_eq!(v_pdag.parents_of(1).unwrap(), &[0]);
        assert_eq!(v_pdag.children_of(0).unwrap(), &[1]);
        assert_eq!(v_pdag.undirected_of(1).unwrap(), &[2]);
        assert_eq!(v_pdag.neighbors_of(1).unwrap(), &[0, 2, 3]);
        assert_eq!(v_pdag.descendants_of(0).unwrap(), vec![1, 3]);
        assert_eq!(v_pdag.ancestors_of(3).unwrap(), vec![0, 1]);
        let v_pdag_core = v_pdag.core();
        assert_eq!(v_pdag_core.n(), 4);

        // Raw view uses UNKNOWN class fallbacks
        let v_raw = GraphView::Raw(pdag_core);
        assert_eq!(v_raw.n(), 4);
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
        assert_eq!(v_raw_core.n(), 4);
    }

    #[test]
    fn graphview_mb_dispatch() {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 3, d).unwrap();
        let dag = Arc::new(Dag::new(Arc::new(b.finalize_in_place().unwrap())).unwrap());
        let v = GraphView::Dag(dag);
        assert_eq!(v.markov_blanket_of(0).unwrap(), vec![1, 2, 3]);

        // Pdag:
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        let u = r.code_of("---").unwrap();
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 3, d).unwrap();
        b.add_edge(1, 3, u).unwrap();
        let pdag = Arc::new(Pdag::new(Arc::new(b.finalize_in_place().unwrap())).unwrap());

        let v = GraphView::Pdag(pdag);
        assert_eq!(v.markov_blanket_of(0).unwrap(), vec![1, 2, 3]);

        // Raw view uses UNKNOWN class fallbacks
        let v = GraphView::Raw(Arc::new(b.finalize().unwrap()));
        assert_eq!(
            v.markov_blanket_of(0).unwrap_err(),
            "markov_blanket_of not implemented for UNKNOWN class"
        );
    }

    #[test]
    fn exogenous_nodes_dag_pdag() {
        use crate::{edges::EdgeRegistry, graph::builder::GraphBuilder};
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let u = r.code_of("---").unwrap();

        // DAG: 0->1, 0->2  => exogenous {0,3}
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        let v = GraphView::Dag(Arc::new(Dag::new(Arc::new(b.finalize().unwrap())).unwrap()));
        assert_eq!(v.exogenous_nodes(false).unwrap(), vec![0, 3]);

        // PDAG: 0->1, 1--2 => exogenous {0,2,3} if undirected ignored; {0,3} if counted
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        let v = GraphView::Pdag(Arc::new(
            Pdag::new(Arc::new(b.finalize().unwrap())).unwrap(),
        ));
        assert_eq!(v.exogenous_nodes(false).unwrap(), vec![0, 2, 3]);
        assert_eq!(v.exogenous_nodes(true).unwrap(), vec![0, 3]);
    }
    #[test]
    fn graphview_dag_adjustment_and_dsep_dispatch() {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();

        // DAG: 0:L -> 1:A -> 2:Y and 0:L -> 2:Y (classic confounder)
        let mut b = GraphBuilder::new_with_registry(3, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        let v = GraphView::Dag(Arc::new(Dag::new(Arc::new(b.finalize().unwrap())).unwrap()));

        // d-sep: X=1, Y=2 is not separated unconditionally; conditioning on 0 separates them
        assert_eq!(v.d_separated(&[1], &[2], &[]).unwrap(), false);
        assert_eq!(v.d_separated(&[0], &[2], &[0]).unwrap(), true);

        // adjustment sets (parents/backdoor/optimal) all return {0}
        assert_eq!(v.adjustment_set_parents(&[1], &[2]).unwrap(), vec![0]);
        assert_eq!(v.adjustment_set_backdoor(&[1], &[2]).unwrap(), vec![0]);
        assert_eq!(v.adjustment_set_optimal(1, 2).unwrap(), vec![0]);

        // backdoor validity + enumeration
        assert_eq!(v.is_valid_backdoor_set(1, 2, &[0]).unwrap(), true);
        assert_eq!(v.is_valid_backdoor_set(1, 2, &[]).unwrap(), false);
        assert_eq!(v.all_backdoor_sets(1, 2, true, 5).unwrap(), vec![vec![0]]);
    }

    // --- d-sep simple chain to hit both true/false via GraphView::Dag ---
    #[test]
    fn graphview_dag_dsep_chain_true_false() {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();

        // 0 -> 1 -> 2
        let mut b = GraphBuilder::new_with_registry(3, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let v = GraphView::Dag(Arc::new(Dag::new(Arc::new(b.finalize().unwrap())).unwrap()));

        // Unblocked path 0—1—2 => not separated
        assert_eq!(v.d_separated(&[0], &[2], &[]).unwrap(), false);
        // Condition on middle node blocks it
        assert_eq!(v.d_separated(&[0], &[2], &[1]).unwrap(), true);
    }

    // --- PDAG and RAW: all DAG-only methods must return errors ---
    #[test]
    fn graphview_pdag_and_raw_dagonly_methods_error() {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let u = r.code_of("---").unwrap();

        // Build a small PDAG: 0->1, 1---2
        let mut bp = GraphBuilder::new_with_registry(3, true, &r);
        bp.add_edge(0, 1, d).unwrap();
        bp.add_edge(1, 2, u).unwrap();
        let pdag_core = Arc::new(bp.finalize().unwrap());
        let vp = GraphView::Pdag(Arc::new(Pdag::new(pdag_core.clone()).unwrap()));
        let vr = GraphView::Raw(pdag_core);

        // d_separated
        assert_eq!(
            vp.d_separated(&[0], &[2], &[]).unwrap_err(),
            "d_separated is only defined for DAGs"
        );
        assert_eq!(
            vr.d_separated(&[0], &[2], &[]).unwrap_err(),
            "d_separated is only defined for DAGs"
        );

        // adjustment_set_parents
        assert_eq!(
            vp.adjustment_set_parents(&[0], &[1]).unwrap_err(),
            "adjustment_set_parents is only defined for DAGs"
        );
        assert_eq!(
            vr.adjustment_set_parents(&[0], &[1]).unwrap_err(),
            "adjustment_set_parents is only defined for DAGs"
        );

        // adjustment_set_backdoor
        assert_eq!(
            vp.adjustment_set_backdoor(&[0], &[1]).unwrap_err(),
            "adjustment_set_backdoor is only defined for DAGs"
        );
        assert_eq!(
            vr.adjustment_set_backdoor(&[0], &[1]).unwrap_err(),
            "adjustment_set_backdoor is only defined for DAGs"
        );

        // adjustment_set_optimal
        assert_eq!(
            vp.adjustment_set_optimal(0, 1).unwrap_err(),
            "adjustment_set_optimal is only defined for DAGs"
        );
        assert_eq!(
            vr.adjustment_set_optimal(0, 1).unwrap_err(),
            "adjustment_set_optimal is only defined for DAGs"
        );

        // is_valid_backdoor_set
        assert_eq!(
            vp.is_valid_backdoor_set(0, 1, &[]).unwrap_err(),
            "is_valid_backdoor_set is only defined for DAGs"
        );
        assert_eq!(
            vr.is_valid_backdoor_set(0, 1, &[]).unwrap_err(),
            "is_valid_backdoor_set is only defined for DAGs"
        );

        // all_backdoor_sets
        assert_eq!(
            vp.all_backdoor_sets(0, 1, true, 5).unwrap_err(),
            "all_backdoor_sets is only defined for DAGs"
        );
        assert_eq!(
            vr.all_backdoor_sets(0, 1, true, 5).unwrap_err(),
            "all_backdoor_sets is only defined for DAGs"
        );
    }

    #[test]
    fn graphview_proper_backdoor_graph_public() {
        // Build L→A→Y with L→Y (classic confounder)
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        // 0:L, 1:A, 2:Y
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap(); // L->A
        b.add_edge(1, 2, d).unwrap(); // A->Y
        b.add_edge(0, 2, d).unwrap(); // L->Y
        let dag = Arc::new(Dag::new(Arc::new(b.finalize().unwrap())).unwrap());
        let v = GraphView::Dag(dag);

        // Get proper backdoor graph for A → Y
        let pbg = v.proper_backdoor_graph(&[1], &[2]).unwrap();

        // In the proper backdoor graph, A->Y edge should be removed
        match pbg {
            GraphView::Dag(ref d) => {
                // Y should only have L as parent
                assert_eq!(d.parents_of(2), &[0]);
                // A and Y should NOT be d-separated without conditioning
                // (backdoor path A <- L -> Y remains)
                assert!(!d.d_separated(&[1], &[2], &[]));
            }
            _ => panic!("Expected DAG"),
        }
    }

    #[test]
    fn graphview_moral_of_ancestors_public() {
        // Build L→A→Y with L→Y
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        // 0:L, 1:A, 2:Y
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap(); // L->A
        b.add_edge(1, 2, d).unwrap(); // A->Y
        b.add_edge(0, 2, d).unwrap(); // L->Y
        let dag = Arc::new(Dag::new(Arc::new(b.finalize().unwrap())).unwrap());
        let v = GraphView::Dag(dag);

        // Get moral graph of ancestors of {A, Y}
        let moral = v.moral_of_ancestors(&[1, 2]).unwrap();

        // Should be a UG with all three nodes
        match moral {
            GraphView::Ug(ref u) => {
                assert_eq!(u.n(), 3);
                // L should be adjacent to A (parent of A)
                // L should be adjacent to Y (parent of Y)
                // A should be adjacent to Y (A->Y in original, also married via Y)
                let mut l_nb = u.neighbors_of(0).to_vec();
                l_nb.sort();
                assert_eq!(l_nb, vec![1, 2]);
            }
            _ => panic!("Expected UG"),
        }
    }

    #[test]
    fn graphview_ancestral_reduction_public() {
        // Build larger DAG: 0->1->2, 3->4 (3,4 are distractors)
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(3, 4, d).unwrap();
        let dag = Arc::new(Dag::new(Arc::new(b.finalize().unwrap())).unwrap());
        let v = GraphView::Dag(dag);

        // Ancestral reduction on {1, 2}
        let reduced = v.ancestral_reduction(&[1, 2]).unwrap();

        // Should only contain An({1,2}) ∪ {1,2} = {0,1,2}
        match reduced {
            GraphView::Dag(ref d) => {
                assert_eq!(d.n(), 3); // nodes {0,1,2}
            }
            _ => panic!("Expected DAG"),
        }
    }

    #[test]
    fn graphview_skeleton_from_dag_and_pdag_and_errors() {
        use crate::graph::ug::Ug;
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let u = r.code_of("---").unwrap();

        // DAG: 0->1, 1->2  ⇒ skeleton neighbors {0:[1], 1:[0,2], 2:[1]}
        let mut bd = GraphBuilder::new_with_registry(3, true, &r);
        bd.add_edge(0, 1, d).unwrap();
        bd.add_edge(1, 2, d).unwrap();
        let v_dag = GraphView::Dag(Arc::new(
            Dag::new(Arc::new(bd.finalize().unwrap())).unwrap(),
        ));
        let ug = match v_dag.skeleton().unwrap() {
            GraphView::Ug(g) => g,
            _ => panic!("expected UG"),
        };
        assert_eq!(ug.neighbors_of(0), &[1]);
        assert_eq!(ug.neighbors_of(1), &[0, 2]);
        assert_eq!(ug.neighbors_of(2), &[1]);

        // PDAG: 0->1, 1--2  ⇒ skeleton neighbors {0:[1], 1:[0,2], 2:[1]}
        let mut bp = GraphBuilder::new_with_registry(3, true, &r);
        bp.add_edge(0, 1, d).unwrap();
        bp.add_edge(1, 2, u).unwrap();
        let v_pdag = GraphView::Pdag(Arc::new(
            Pdag::new(Arc::new(bp.finalize().unwrap())).unwrap(),
        ));
        let ug2 = match v_pdag.skeleton().unwrap() {
            GraphView::Ug(g) => g,
            _ => panic!("expected UG"),
        };
        assert_eq!(ug2.neighbors_of(0), &[1]);
        assert_eq!(ug2.neighbors_of(1), &[0, 2]);
        assert_eq!(ug2.neighbors_of(2), &[1]);

        // UG: calling skeleton() must error
        let mut bu = GraphBuilder::new_with_registry(2, true, &r);
        bu.add_edge(0, 1, u).unwrap();
        let v_ug = GraphView::Ug(Arc::new(Ug::new(Arc::new(bu.finalize().unwrap())).unwrap()));
        assert_eq!(
            v_ug.skeleton().unwrap_err(),
            "skeleton is defined for DAGs and PDAGs only"
        );
    }

    #[test]
    fn graphview_moralize_from_dag_and_errors() {
        use crate::graph::ug::Ug;
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let u = r.code_of("---").unwrap();

        // Moralize v-structure: 0->2<-1  ⇒ marry parents ⇒ UG edges {0-2,1-2,0-1}
        let mut b = GraphBuilder::new_with_registry(3, true, &r);
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let v_dag = GraphView::Dag(Arc::new(Dag::new(Arc::new(b.finalize().unwrap())).unwrap()));
        let ug = match v_dag.moralize().unwrap() {
            GraphView::Ug(g) => g,
            _ => panic!("expected UG"),
        };
        assert_eq!(ug.neighbors_of(0), &[1, 2]);
        assert_eq!(ug.neighbors_of(1), &[0, 2]);
        assert_eq!(ug.neighbors_of(2), &[0, 1]);

        // PDAG: moralize() must error
        let mut bp = GraphBuilder::new_with_registry(2, true, &r);
        bp.add_edge(0, 1, d).unwrap();
        let v_pdag = GraphView::Pdag(Arc::new(
            Pdag::new(Arc::new(bp.finalize().unwrap())).unwrap(),
        ));
        assert_eq!(
            v_pdag.moralize().unwrap_err(),
            "moralize is only defined for DAGs"
        );

        // UG: moralize() must error
        let mut bu = GraphBuilder::new_with_registry(2, true, &r);
        bu.add_edge(0, 1, u).unwrap();
        let v_ug = GraphView::Ug(Arc::new(Ug::new(Arc::new(bu.finalize().unwrap())).unwrap()));
        assert_eq!(
            v_ug.moralize().unwrap_err(),
            "moralize is only defined for DAGs"
        );
    }
}
