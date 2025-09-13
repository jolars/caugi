// SPDX-License-Identifier: MIT
use extendr_api::prelude::*;

pub mod edges;
use edges::{EdgeRegistry, EdgeSpec, QueryFlags, 
    parse_mark, parse_orientation, parse_class};

#[extendr] 
fn edge_registry_new() -> ExternalPtr<EdgeRegistry> { 
    ExternalPtr::new(EdgeRegistry::new()) 
}

#[extendr] 
fn edge_registry_register_builtins(mut reg: ExternalPtr<EdgeRegistry>) {
    if let Err(e) = reg.as_mut().register_builtins() { 
        throw_r_error(format!("{}", e)); 
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
    glyph:&str, left_mark:&str, right_mark:&str,
    orientation:&str, class:&str, symmetric:bool, traversable_when_conditioned:bool,
) -> i32 {
    let spec = EdgeSpec {
        glyph: glyph.to_string(),
        left_mark: parse_mark(left_mark).unwrap_or_else(|e| throw_r_error(e.to_string())),
        right_mark: parse_mark(right_mark).unwrap_or_else(|e| throw_r_error(e.to_string())),
        orientation: parse_orientation(orientation).unwrap_or_else(|e| throw_r_error(e.to_string())),
        class: parse_class(class).unwrap_or_else(|e| throw_r_error(e.to_string())),
        symmetric,
        flags: if traversable_when_conditioned { QueryFlags::TRAVERSABLE_WHEN_CONDITIONED } else { QueryFlags::empty() },
    };
    match reg.as_mut().register(spec) {
        Ok(c) => c as i32,
        Err(e) => throw_r_error(e.to_string())
    }

}

extendr_module! {
    mod caugi;
    fn edge_registry_new;
    fn edge_registry_register_builtins;
    fn edge_registry_seal;
    fn edge_registry_len;
    fn edge_registry_register;
}
