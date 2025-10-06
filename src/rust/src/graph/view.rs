use super::CaugiGraph;
use super::dag::Dag;
use super::pdag::Pdag;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum GraphView {
    Dag(Arc<Dag>),
    Pdag(Arc<Pdag>),
    Raw(Arc<CaugiGraph>),
    // ADMG, MAG, PAG, etc in the future
}

impl GraphView {
    #[inline]
    pub fn core(&self) -> &CaugiGraph {
        match self {
            GraphView::Dag(d) => d.core_ref(),
            GraphView::Pdag(p) => p.core_ref(),
            GraphView::Raw(c) => c,
        }
    }

    #[inline]
    pub fn n(&self) -> u32 {
        match self {
            GraphView::Dag(g) => g.n(),
            GraphView::Pdag(g) => g.n(),
            GraphView::Raw(core) => core.n(),
        }
    }

    // ---- queries ----
    pub fn parents_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.parents_of(i)),
            GraphView::Pdag(g) => Ok(g.parents_of(i)),
            GraphView::Raw(_) => Err("parents_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn children_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.children_of(i)),
            GraphView::Pdag(g) => Ok(g.children_of(i)),
            GraphView::Raw(_) => Err("children_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn undirected_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(_) => Err("undirected_of not defined for Dag".into()),
            GraphView::Pdag(g) => Ok(g.undirected_of(i)),
            GraphView::Raw(_) => Err("undirected_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn neighbors_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.neighbors_of(i)),
            GraphView::Pdag(g) => Ok(g.neighbors_of(i)),
            GraphView::Raw(_) => Err("neighbors_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn ancestors_of(&self, i: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.ancestors_of(i)),
            GraphView::Pdag(g) => Ok(g.ancestors_of(i)),
            GraphView::Raw(_) => Err("ancestors_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn descendants_of(&self, i: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.descendants_of(i)),
            GraphView::Pdag(g) => Ok(g.descendants_of(i)),
            GraphView::Raw(_) => Err("descendants_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn markov_blanket_of(&self, i: u32) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.markov_blanket_of(i)),
            GraphView::Pdag(g) => Ok(g.markov_blanket_of(i)),
            GraphView::Raw(_) => Err("markov_blanket_of not implemented for UNKNOWN class".into()),
        }
    }
    pub fn exogenous_nodes(&self, undirected_as_parents: bool) -> Result<Vec<u32>, String> {
        match self {
            GraphView::Dag(g) => Ok(g.exogenous_nodes()),
            GraphView::Pdag(g) => Ok(g.exogenous_nodes(undirected_as_parents)),
            GraphView::Raw(_) => Err("exogenous_nodes not implemented for UNKNOWN class".into()),
        }
    }

    // ---- DAG-only methods ----
    pub fn is_d_separated(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> Result<bool, String> {
        match self {
            GraphView::Dag(d) => Ok(d.is_d_separated(xs, ys, z)),
            _ => Err("is_d_separated is only defined for DAGs".into()),
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
}
