// SPDX-License-Identifier: MIT
//! R bindings for graph builder operations.

use extendr_api::prelude::*;
use std::sync::Arc;

use super::helpers::{rint_to_u32, rint_to_u8};
use crate::edges::EdgeRegistry;
use crate::graph::builder::GraphBuilder;
use crate::graph::view::GraphView;
use crate::graph::{admg::Admg, dag::Dag, pdag::Pdag, ug::Ug, CaugiGraph};

#[extendr]
pub fn graph_builder_new(
    reg: ExternalPtr<EdgeRegistry>,
    n: i32,
    simple: Rbool,
) -> ExternalPtr<GraphBuilder> {
    if n < 0 {
        throw_r_error("n must be >= 0");
    }
    ExternalPtr::new(GraphBuilder::new(n as u32, simple.is_true(), reg.as_ref()))
}

#[extendr]
pub fn graph_builder_add_edges(
    mut b: ExternalPtr<GraphBuilder>,
    from: Integers,
    to: Integers,
    etype: Integers,
) {
    if from.len() != to.len() || from.len() != etype.len() {
        throw_r_error("vectors must have equal length");
    }
    for i in 0..from.len() {
        let u = rint_to_u32(from[i], "from");
        let v = rint_to_u32(to[i], "to");
        let t = rint_to_u8(etype[i], "etype");
        if let Err(e) = b.as_mut().add_edge(u, v, t) {
            throw_r_error(e);
        }
    }
}

/// Create a GraphView from a core and class string.
pub(crate) fn graphview_new(
    core: ExternalPtr<CaugiGraph>,
    class: &str,
) -> ExternalPtr<GraphView> {
    match class.trim().to_ascii_uppercase().as_str() {
        "DAG" => {
            let dag =
                Dag::new(Arc::new(core.as_ref().clone())).unwrap_or_else(|e| throw_r_error(e));
            ExternalPtr::new(GraphView::Dag(Arc::new(dag)))
        }
        "PDAG" | "CPDAG" => {
            let pdag =
                Pdag::new(Arc::new(core.as_ref().clone())).unwrap_or_else(|e| throw_r_error(e));
            ExternalPtr::new(GraphView::Pdag(Arc::new(pdag)))
        }
        "UG" => {
            let ug = Ug::new(Arc::new(core.as_ref().clone())).unwrap_or_else(|e| throw_r_error(e));
            ExternalPtr::new(GraphView::Ug(Arc::new(ug)))
        }
        "ADMG" => {
            let admg =
                Admg::new(Arc::new(core.as_ref().clone())).unwrap_or_else(|e| throw_r_error(e));
            ExternalPtr::new(GraphView::Admg(Arc::new(admg)))
        }
        _ => ExternalPtr::new(GraphView::Raw(Arc::new(core.as_ref().clone()))),
    }
}

#[extendr]
pub fn graph_builder_build_view(
    mut b: ExternalPtr<GraphBuilder>,
    class: &str,
) -> ExternalPtr<GraphView> {
    let core = b
        .as_mut()
        .finalize_in_place()
        .unwrap_or_else(|e| throw_r_error(e));
    graphview_new(ExternalPtr::new(core), class)
}

extendr_module! {
    mod builder;
    fn graph_builder_new;
    fn graph_builder_add_edges;
    fn graph_builder_build_view;
}

