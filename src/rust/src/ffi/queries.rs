// SPDX-License-Identifier: MIT
//! R bindings for graph query operations.

use extendr_api::prelude::*;

use super::helpers::rint_to_u32;
use crate::graph::view::GraphView;

#[extendr]
pub fn parents_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .parents_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.into_iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn children_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .children_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.into_iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn undirected_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .undirected_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.into_iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn neighbors_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .neighbors_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.into_iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn ancestors_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .ancestors_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn descendants_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .descendants_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn markov_blanket_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .markov_blanket_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn exogenous_nodes_of_ptr(g: ExternalPtr<GraphView>, undirected_as_parents: Rbool) -> Robj {
    let undirected_as_parents = undirected_as_parents.is_true();
    g.as_ref()
        .exogenous_nodes(undirected_as_parents)
        .map(|s| s.iter().map(|&x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn spouses_of_ptr(g: ExternalPtr<GraphView>, idxs: Integers) -> Robj {
    let mut out: Vec<Robj> = Vec::with_capacity(idxs.len());
    for ri in idxs.iter() {
        let i = rint_to_u32(ri, "idxs");
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i));
        }
        let v = g
            .as_ref()
            .spouses_of(i)
            .unwrap_or_else(|e| throw_r_error(e));
        out.push(v.iter().map(|&x| x as i32).collect_robj());
    }
    extendr_api::prelude::List::from_values(out).into_robj()
}

#[extendr]
pub fn districts_ptr(g: ExternalPtr<GraphView>) -> Robj {
    let districts = g.as_ref().districts().unwrap_or_else(|e| throw_r_error(e));
    let robjs: Vec<Robj> = districts
        .into_iter()
        .map(|v| v.into_iter().map(|u| u as i32).collect_robj())
        .collect();
    extendr_api::prelude::List::from_values(robjs).into_robj()
}

#[extendr]
pub fn district_of_ptr(g: ExternalPtr<GraphView>, idx: i32) -> Robj {
    if idx < 0 {
        throw_r_error("idx must be >= 0");
    }
    let i = idx as u32;
    if i >= g.as_ref().n() {
        throw_r_error(format!("Index {} is out of bounds", i));
    }
    let v = g
        .as_ref()
        .district_of(i)
        .unwrap_or_else(|e| throw_r_error(e));
    v.into_iter().map(|x| x as i32).collect_robj()
}

#[extendr]
pub fn induced_subgraph_ptr(g: ExternalPtr<GraphView>, keep: Integers) -> Robj {
    let mut ks: Vec<u32> = Vec::with_capacity(keep.len());
    for ri in keep.iter() {
        let u = rint_to_u32(ri, "keep");
        if u >= g.as_ref().n() {
            throw_r_error(format!("node id {} out of bounds", u));
        }
        ks.push(u);
    }

    let sub = g
        .as_ref()
        .induced_subgraph(&ks)
        .unwrap_or_else(|e| throw_r_error(e));

    let sub_ptr = ExternalPtr::new(sub);
    sub_ptr.into_robj()
}

#[extendr]
pub fn n_ptr(g: ExternalPtr<GraphView>) -> i32 {
    g.as_ref().n() as i32
}

#[extendr]
pub fn edges_ptr_df(g: ExternalPtr<GraphView>) -> Robj {
    let core = g.as_ref().core();
    let n = core.n();
    let mut from0: Vec<i32> = Vec::new();
    let mut to0: Vec<i32> = Vec::new();
    let mut code: Vec<i32> = Vec::new();
    let mut glyph: Vec<String> = Vec::new();

    for u in 0..n {
        for k in core.row_range(u) {
            let v = core.col_index[k];
            let ecode = core.etype[k];
            let spec = &core.registry.specs[ecode as usize];
            if spec.symmetric {
                if u < v {
                    from0.push(u as i32);
                    to0.push(v as i32);
                    code.push(ecode as i32);
                    glyph.push(spec.glyph.clone());
                }
            } else if core.side[k] == 0 {
                from0.push(u as i32);
                to0.push(v as i32);
                code.push(ecode as i32);
                glyph.push(spec.glyph.clone());
            }
        }
    }
    list!(from0 = from0, to0 = to0, code = code, glyph = glyph).into_robj()
}

#[extendr]
pub fn is_simple_ptr(g: ExternalPtr<GraphView>) -> bool {
    g.as_ref().core().simple
}

#[extendr]
pub fn graph_class_ptr(g: ExternalPtr<GraphView>) -> String {
    match g.as_ref() {
        GraphView::Dag(_) => "DAG",
        GraphView::Pdag(_) => "PDAG",
        GraphView::Ug(_) => "UG",
        GraphView::Admg(_) => "ADMG",
        GraphView::Raw(_) => "UNKNOWN",
    }
    .to_string()
}

#[extendr]
pub fn is_acyclic_ptr(g: ExternalPtr<GraphView>) -> bool {
    let core = g.as_ref().core();
    crate::graph::alg::directed_part_is_acyclic(core)
}

#[extendr]
pub fn is_dag_type_ptr(g: ExternalPtr<GraphView>) -> bool {
    use crate::graph::dag::Dag;
    use std::sync::Arc;
    let core = g.as_ref().core();
    Dag::new(Arc::new(core.clone())).is_ok()
}

#[extendr]
pub fn is_pdag_type_ptr(g: ExternalPtr<GraphView>) -> bool {
    use crate::graph::pdag::Pdag;
    use std::sync::Arc;
    let core = g.as_ref().core();
    Pdag::new(Arc::new(core.clone())).is_ok()
}

#[extendr]
pub fn is_ug_type_ptr(g: ExternalPtr<GraphView>) -> bool {
    use crate::graph::ug::Ug;
    use std::sync::Arc;
    let core = g.as_ref().core();
    Ug::new(Arc::new(core.clone())).is_ok()
}

#[extendr]
pub fn is_admg_type_ptr(g: ExternalPtr<GraphView>) -> bool {
    use crate::graph::admg::Admg;
    use std::sync::Arc;
    let core = g.as_ref().core();
    Admg::new(Arc::new(core.clone())).is_ok()
}

extendr_module! {
    mod queries;
    fn parents_of_ptr;
    fn children_of_ptr;
    fn undirected_of_ptr;
    fn neighbors_of_ptr;
    fn ancestors_of_ptr;
    fn descendants_of_ptr;
    fn markov_blanket_of_ptr;
    fn exogenous_nodes_of_ptr;
    fn spouses_of_ptr;
    fn districts_ptr;
    fn district_of_ptr;
    fn induced_subgraph_ptr;
    fn n_ptr;
    fn edges_ptr_df;
    fn is_simple_ptr;
    fn graph_class_ptr;
    fn is_acyclic_ptr;
    fn is_dag_type_ptr;
    fn is_pdag_type_ptr;
    fn is_ug_type_ptr;
    fn is_admg_type_ptr;
}

