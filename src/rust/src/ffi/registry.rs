// SPDX-License-Identifier: MIT
//! R bindings for edge registry operations.

use extendr_api::prelude::*;

use crate::edges::{EdgeClass, EdgeRegistry, EdgeSpec, Mark};

#[extendr]
pub fn edge_registry_new() -> ExternalPtr<EdgeRegistry> {
    ExternalPtr::new(EdgeRegistry::new())
}

#[extendr]
pub fn edge_registry_register_builtins(mut reg: ExternalPtr<EdgeRegistry>) {
    if let Err(e) = reg.as_mut().register_builtins() {
        throw_r_error(e.to_string());
    }
}

#[extendr]
pub fn edge_registry_seal(mut reg: ExternalPtr<EdgeRegistry>) {
    reg.as_mut().seal();
}

#[extendr]
pub fn edge_registry_len(reg: ExternalPtr<EdgeRegistry>) -> i32 {
    reg.as_ref().len() as i32
}

#[extendr]
pub fn edge_registry_register(
    mut reg: ExternalPtr<EdgeRegistry>,
    glyph: &str,
    tail_mark: &str,
    head_mark: &str,
    class: &str,
    symmetric: bool,
) -> i32 {
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
    };
    match reg.as_mut().register(spec) {
        Ok(c) => c as i32,
        Err(e) => throw_r_error(e.to_string()),
    }
}

#[extendr]
pub fn edge_registry_code_of(reg: ExternalPtr<EdgeRegistry>, glyphs: Strings) -> Robj {
    let mut out: Vec<i32> = Vec::with_capacity(glyphs.len());
    for g in glyphs.iter() {
        let code = reg
            .as_ref()
            .code_of(g.as_str())
            .unwrap_or_else(|e| throw_r_error(e.to_string()));
        out.push(code as i32);
    }
    out.into_robj()
}

#[extendr]
pub fn edge_registry_spec_of_code(reg: ExternalPtr<EdgeRegistry>, code: i32) -> Robj {
    if !(0..=255).contains(&code) {
        throw_r_error("code must be in 0..=255");
    }
    match reg.as_ref().spec_of_code(code as u8) {
        Ok(spec) => list!(
            glyph = spec.glyph.to_string(),
            tail = spec.tail.to_string(),
            head = spec.head.to_string(),
            class = spec.class.to_string(),
            symmetric = spec.symmetric
        )
        .into_robj(),
        Err(e) => throw_r_error(e.to_string()),
    }
}

extendr_module! {
    mod registry;
    fn edge_registry_new;
    fn edge_registry_register_builtins;
    fn edge_registry_seal;
    fn edge_registry_len;
    fn edge_registry_register;
    fn edge_registry_code_of;
    fn edge_registry_spec_of_code;
}

