// SPDX-License-Identifier: MIT
use extendr_api::prelude::*;

pub mod edges;
pub mod graph;

use edges::{EdgeRegistry, EdgeSpec, QueryFlags, parse_orientation, parse_class};
use graph::CaugiGraph;
use graph::builder::GraphBuilder;

// helpers
fn rint_to_u32(x: Rint, field: &str) -> u32 {
    if x.is_na() { throw_r_error(format!("NA in `{}`", field)); }
    let v = x.inner(); // i32
    if v < 0 { throw_r_error(format!("`{}` must be >= 0", field)); }
    v as u32
}
fn rint_to_u8(x: Rint, field: &str) -> u8 {
    if x.is_na() { throw_r_error(format!("NA in `{}`", field)); }
    let v = x.inner();
    if !(0..=255).contains(&v) { throw_r_error(format!("`{}` must be in 0..=255", field)); }
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
    orientation: &str,
    class: &str,
    symmetric: bool,
    flags: Vec<String>, // e.g. c("TAIL_UNDIR","HEAD_POSS_PARENT")
) -> i32 {
    use QueryFlags as F;

    fn parse_flags(v: &[String]) -> F {
        let mut out = F::empty();
        for raw in v {
            match raw.trim().to_ascii_uppercase().as_str() {
                "TAIL_PARENT" => out |= F::TAIL_PARENT,
                "TAIL_CHILD" => out |= F::TAIL_CHILD,
                "TAIL_UNDIR" => out |= F::TAIL_UNDIR,
                "TAIL_UNKNOWN" => out |= F::TAIL_UNKNOWN,
                "HEAD_PARENT" => out |= F::HEAD_PARENT,
                "HEAD_CHILD" => out |= F::HEAD_CHILD,
                "HEAD_UNDIR" => out |= F::HEAD_UNDIR,
                "HEAD_UNKNOWN" => out |= F::HEAD_UNKNOWN,
                "LATENT_CONFOUNDING" => out |= F::LATENT_CONFOUNDING,
                "HEAD_POSS_CHILD" => out |= F::HEAD_POSS_CHILD,
                "HEAD_POSS_PARENT" => out |= F::HEAD_POSS_PARENT,
                "TAIL_POSS_PARENT" => out |= F::TAIL_POSS_PARENT,
                "TAIL_POSS_CHILD" => out |= F::TAIL_POSS_CHILD,
                "TRAVERSABLE_WHEN_CONDITIONED" | "TRAVERSABLE" => {
                    out |= F::TRAVERSABLE_WHEN_CONDITIONED
                }
                other => throw_r_error(format!("Unknown flag '{other}'")),
            }
        }
        out
    }

    let spec = EdgeSpec {
        glyph: glyph.to_string(),
        orientation: parse_orientation(orientation).unwrap_or_else(|e| throw_r_error(e)),
        class: parse_class(class).unwrap_or_else(|e| throw_r_error(e)),
        symmetric,
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

// ── Graph builder + queries ─────────────────────────────────────────────────

#[extendr]
fn graph_builder_new(reg: ExternalPtr<EdgeRegistry>, n: i32, simple: Rbool) -> ExternalPtr<GraphBuilder> {
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

#[extendr] 
fn parents_of_ptr(g: ExternalPtr<CaugiGraph>, i:i32) -> Robj {
    g.as_ref().parents_of(i as u32).iter().map(|&x| x as i32).collect_robj()
}

#[extendr] 
fn children_of_ptr(g: ExternalPtr<CaugiGraph>, i:i32) -> Robj {
    g.as_ref().children_of(i as u32).iter().map(|&x| x as i32).collect_robj()
}

#[extendr] 
fn undirected_unknown_of_ptr(g: ExternalPtr<CaugiGraph>, i:i32) -> Robj {
    g.as_ref().adjacent_undirected_unknown_of(i as u32).iter().map(|&x| x as i32).collect_robj()
}

#[extendr] 
fn possible_parents_of_ptr(g: ExternalPtr<CaugiGraph>, i:i32) -> Robj {
    g.as_ref().possible_parents_of(i as u32).into_iter().map(|x| x as i32).collect_robj()
}

#[extendr] 
fn possible_children_of_ptr(g: ExternalPtr<CaugiGraph>, i:i32) -> Robj {
    g.as_ref().possible_children_of(i as u32).into_iter().map(|x| x as i32).collect_robj()
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
    
    // builder + graph
    fn graph_builder_new; 
    fn graph_builder_add_edges; 
    fn graph_builder_build;

    // queries
    fn parents_of_ptr; 
    fn children_of_ptr; 
    fn undirected_unknown_of_ptr;
    fn possible_parents_of_ptr; 
    fn possible_children_of_ptr;
}
