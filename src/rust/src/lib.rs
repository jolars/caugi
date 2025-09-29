// SPDX-License-Identifier: MIT
//! R bindings for caugi graph library.

use extendr_api::prelude::*;
pub mod edges;
pub mod graph;

use edges::{EdgeClass, EdgeRegistry, EdgeSpec, Mark, QueryFlags};
use graph::CaugiGraph;
use graph::builder::GraphBuilder;
use graph::graph_type::GraphType;
use graph::view::{GraphApi, GraphView};
use graph::{dag::Dag, pdag::Pdag};
use std::sync::Arc;

// ---------- helpers ----------
fn rint_to_u32(x: Rint, field: &str) -> u32 {
    if x.is_na() {
        throw_r_error(format!("NA in `{}`", field));
    }
    let v = x.inner();
    if v < 0 {
        throw_r_error(format!("`{}` must be >= 0", field));
    }
    v as u32
}
fn rint_to_u8(x: Rint, field: &str) -> u8 {
    if x.is_na() {
        throw_r_error(format!("NA in `{}`", field));
    }
    let v = x.inner();
    if !(0..=255).contains(&v) {
        throw_r_error(format!("`{}` must be in 0..=255", field));
    }
    v as u8
}

// ── Edge Registry  ────────────────────────────────────────────────────────────────

#[extendr]
fn edge_registry_new() -> ExternalPtr<EdgeRegistry> {
    ExternalPtr::new(EdgeRegistry::new())
}

#[extendr]
fn edge_registry_register_builtins(mut reg: ExternalPtr<EdgeRegistry>) {
    if let Err(e) = reg.as_mut().register_builtins() {
        throw_r_error(e.to_string());
    }
}

#[extendr]
fn edge_registry_seal(mut reg: ExternalPtr<EdgeRegistry>) {
    reg.as_mut().seal();
}

#[extendr]
fn edge_registry_len(reg: ExternalPtr<EdgeRegistry>) -> i32 {
    reg.as_ref().len() as i32
}

#[extendr]
fn edge_registry_register(
    mut reg: ExternalPtr<EdgeRegistry>,
    glyph: &str,
    tail_mark: &str,
    head_mark: &str,
    class: &str,
    symmetric: bool,
    flags: Vec<String>,
) -> i32 {
    use QueryFlags as F;
    fn parse_flags(v: &[String]) -> F {
        let mut out = F::empty();
        for raw in v {
            match raw.trim().to_ascii_uppercase().as_str() {
                "TRAVERSABLE_WHEN_CONDITIONED" | "TRAVERSABLE" => {
                    out |= F::TRAVERSABLE_WHEN_CONDITIONED
                }
                "LATENT_CONFOUNDING" => out |= F::LATENT_CONFOUNDING,
                other => throw_r_error(format!("Unknown flag '{other}'")),
            }
        }
        out
    }
    let tail = tail_mark
        .parse::<Mark>()
        .unwrap_or_else(|e| throw_r_error(e));
    let head = head_mark
        .parse::<Mark>()
        .unwrap_or_else(|e| throw_r_error(e));
    let class = class
        .parse::<EdgeClass>()
        .unwrap_or_else(|e| throw_r_error(e));
    let spec = EdgeSpec {
        glyph: glyph.to_string(),
        tail,
        head,
        symmetric,
        class,
        flags: parse_flags(&flags),
    };
    match reg.as_mut().register(spec) {
        Ok(c) => c as i32,
        Err(e) => throw_r_error(e.to_string()),
    }
}

#[extendr]
fn edge_registry_code_of(reg: ExternalPtr<EdgeRegistry>, glyph: &str) -> i32 {
    reg.as_ref()
        .code_of(glyph)
        .map(|c| c as i32)
        .unwrap_or_else(|e| throw_r_error(e.to_string()))
}

// ---------- Core builder ----------
#[extendr]
fn graph_builder_new(
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
fn graph_builder_add_edges(
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
#[extendr]
fn graph_builder_build(mut b: ExternalPtr<GraphBuilder>) -> ExternalPtr<CaugiGraph> {
    let g = b
        .as_mut()
        .finalize_in_place()
        .unwrap_or_else(|e| throw_r_error(e));
    ExternalPtr::new(g)
}

// ---------- Constructors for class views (return GraphView) ----------
#[extendr]
fn graphview_new(core: ExternalPtr<CaugiGraph>, class: &str) -> ExternalPtr<GraphView> {
    match class
        .parse::<GraphType>()
        .unwrap_or_else(|e| throw_r_error(e))
    {
        GraphType::Dag => {
            let dag =
                Dag::new(Arc::new(core.as_ref().clone())).unwrap_or_else(|e| throw_r_error(e));
            ExternalPtr::new(GraphView::Dag(Arc::new(dag)))
        }
        GraphType::Pdag => {
            let pdag =
                Pdag::new(Arc::new(core.as_ref().clone())).unwrap_or_else(|e| throw_r_error(e));
            ExternalPtr::new(GraphView::Pdag(Arc::new(pdag)))
        }
        GraphType::Unknown => ExternalPtr::new(GraphView::Raw(Arc::new(core.as_ref().clone()))),
    }
}

#[extendr]
fn graph_builder_build_view(
    mut b: ExternalPtr<GraphBuilder>,
    class: &str,
) -> ExternalPtr<GraphView> {
    let core = b
        .as_mut()
        .finalize_in_place()
        .unwrap_or_else(|e| throw_r_error(e));
    graphview_new(ExternalPtr::new(core), class)
}

// ---------- Unified queries ----------
#[extendr]
fn parents_of_ptr(g: ExternalPtr<GraphView>, i: i32) -> Robj {
    g.as_ref()
        .parents_of(i as u32)
        .map(|s| s.iter().map(|&x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}
#[extendr]
fn children_of_ptr(g: ExternalPtr<GraphView>, i: i32) -> Robj {
    g.as_ref()
        .children_of(i as u32)
        .map(|s| s.iter().map(|&x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}
#[extendr]
fn undirected_of_ptr(g: ExternalPtr<GraphView>, i: i32) -> Robj {
    g.as_ref()
        .undirected_of(i as u32)
        .map(|s| s.iter().map(|&x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
fn neighbors_of_ptr(g: ExternalPtr<GraphView>, i: i32) -> Robj {
    g.as_ref()
        .neighbors_of(i as u32)
        .map(|s| s.iter().map(|&x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
fn ancestors_of_ptr(g: ExternalPtr<GraphView>, i: i32) -> Robj {
    g.as_ref()
        .ancestors_of(i as u32)
        .map(|s| s.iter().map(|x| *x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
fn descendants_of_ptr(g: ExternalPtr<GraphView>, i: i32) -> Robj {
    g.as_ref()
        .descendants_of(i as u32)
        .map(|s| s.iter().map(|x| *x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
fn is_dag_type_ptr(g: ExternalPtr<GraphView>) -> bool {
    let core = g.as_ref().core();
    crate::graph::alg::validate_graph_type(&crate::graph::graph_type::GraphType::Dag, core).is_ok()
}

#[extendr]
fn is_pdag_type_ptr(g: ExternalPtr<GraphView>) -> bool {
    let core = g.as_ref().core();
    crate::graph::alg::validate_graph_type(&crate::graph::graph_type::GraphType::Pdag, core).is_ok()
}

#[extendr]
fn graph_class_ptr(g: ExternalPtr<GraphView>) -> String {
    match g.as_ref() {
        GraphView::Dag(_) => "DAG",
        GraphView::Pdag(_) => "PDAG",
        GraphView::Raw(_) => "UNKNOWN",
    }
    .to_string()
}

#[extendr]
fn is_acyclic_ptr(g: ExternalPtr<GraphView>) -> bool {
    let core = g.as_ref().core();
    crate::graph::alg::directed_part_is_acyclic(core)
}

#[extendr]
fn is_simple_ptr(g: ExternalPtr<GraphView>) -> bool {
    g.as_ref().core().simple
}

extendr_module! {
    mod caugi;
    // registry
    fn edge_registry_new;
    fn edge_registry_register_builtins;
    fn edge_registry_seal;
    fn edge_registry_len;
    fn edge_registry_register;
    fn edge_registry_code_of;

    // builder + core
    fn graph_builder_new;
    fn graph_builder_add_edges;
    fn graph_builder_build;

    // class factory
    fn graphview_new;
    fn graph_builder_build_view;

    // queries
    fn parents_of_ptr;
    fn children_of_ptr;
    fn undirected_of_ptr;
    fn neighbors_of_ptr;
    fn ancestors_of_ptr;
    fn descendants_of_ptr;

    // graph properties
    fn is_simple_ptr;
    fn graph_class_ptr;

    // acyclicity test
    fn is_acyclic_ptr;

    // class tests
    fn is_dag_type_ptr;
    fn is_pdag_type_ptr;
}
