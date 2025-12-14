// SPDX-License-Identifier: MIT
//! R bindings for graph transformation operations.

use extendr_api::prelude::*;
use std::sync::Arc;

use super::helpers::rint_to_u32;
use crate::graph::pdag::Pdag;
use crate::graph::view::GraphView;

#[extendr]
pub fn skeleton_ptr(g: ExternalPtr<GraphView>) -> ExternalPtr<GraphView> {
    let out = g.as_ref().skeleton().unwrap_or_else(|e| throw_r_error(e));
    ExternalPtr::new(out)
}

#[extendr]
pub fn moralize_ptr(g: ExternalPtr<GraphView>) -> ExternalPtr<GraphView> {
    let out = g.as_ref().moralize().unwrap_or_else(|e| throw_r_error(e));
    ExternalPtr::new(out)
}

#[extendr]
pub fn to_cpdag_ptr(g: ExternalPtr<GraphView>) -> ExternalPtr<GraphView> {
    let out = g.as_ref().to_cpdag().unwrap_or_else(|e| throw_r_error(e));
    ExternalPtr::new(out)
}

#[extendr]
pub fn is_cpdag_ptr(g: ExternalPtr<GraphView>) -> bool {
    let core = g.as_ref().core();
    match Pdag::new(Arc::new(core.clone())) {
        Ok(p) => p.is_cpdag(),
        Err(_) => false,
    }
}

#[extendr]
pub fn proper_backdoor_graph_ptr(
    g: ExternalPtr<GraphView>,
    xs: Integers,
    ys: Integers,
) -> ExternalPtr<GraphView> {
    let xs_u: Vec<u32> = xs.iter().map(|ri| rint_to_u32(ri, "xs")).collect();
    let ys_u: Vec<u32> = ys.iter().map(|ri| rint_to_u32(ri, "ys")).collect();
    for &i in xs_u.iter().chain(ys_u.iter()) {
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i + 1));
        }
    }
    let out = g
        .as_ref()
        .proper_backdoor_graph(&xs_u, &ys_u)
        .unwrap_or_else(|e| throw_r_error(e));
    ExternalPtr::new(out)
}

#[extendr]
pub fn moral_of_ancestors_ptr(g: ExternalPtr<GraphView>, seeds: Integers) -> ExternalPtr<GraphView> {
    let seeds_u: Vec<u32> = seeds.iter().map(|ri| rint_to_u32(ri, "seeds")).collect();
    for &i in &seeds_u {
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i + 1));
        }
    }
    let out = g
        .as_ref()
        .moral_of_ancestors(&seeds_u)
        .unwrap_or_else(|e| throw_r_error(e));
    ExternalPtr::new(out)
}

#[extendr]
pub fn ancestral_reduction_ptr(g: ExternalPtr<GraphView>, seeds: Integers) -> ExternalPtr<GraphView> {
    let seeds_u: Vec<u32> = seeds.iter().map(|ri| rint_to_u32(ri, "seeds")).collect();
    for &i in &seeds_u {
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i + 1));
        }
    }
    let out = g
        .as_ref()
        .ancestral_reduction(&seeds_u)
        .unwrap_or_else(|e| throw_r_error(e));
    ExternalPtr::new(out)
}

#[extendr]
pub fn compute_layout_ptr(g: ExternalPtr<GraphView>, method: &str) -> Robj {
    use crate::graph::layout::{compute_layout, LayoutMethod};
    use std::str::FromStr;

    let layout_method = LayoutMethod::from_str(method).unwrap_or_else(|e| throw_r_error(e));

    let coords =
        compute_layout(g.as_ref().core(), layout_method).unwrap_or_else(|e| throw_r_error(e));

    let n = coords.len();
    let mut x = Vec::with_capacity(n);
    let mut y = Vec::with_capacity(n);

    for (xi, yi) in coords {
        x.push(xi);
        y.push(yi);
    }

    list!(x = x, y = y).into_robj()
}

extendr_module! {
    mod transforms;
    fn skeleton_ptr;
    fn moralize_ptr;
    fn to_cpdag_ptr;
    fn is_cpdag_ptr;
    fn proper_backdoor_graph_ptr;
    fn moral_of_ancestors_ptr;
    fn ancestral_reduction_ptr;
    fn compute_layout_ptr;
}

