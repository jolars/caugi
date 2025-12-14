// SPDX-License-Identifier: MIT
//! R bindings for graph metrics (SHD, HD, AID).

use extendr_api::prelude::*;
use std::collections::HashMap;

use crate::graph::metrics::{hd, shd_with_perm};
use crate::graph::view::GraphView;

#[cfg(feature = "gadjid")]
use crate::graph::metrics::aid;

#[extendr]
pub fn shd_of_ptrs(
    g1: ExternalPtr<GraphView>,
    names1: Strings,
    g2: ExternalPtr<GraphView>,
    names2: Strings,
) -> Robj {
    let core1 = g1.as_ref().core();
    let core2 = g2.as_ref().core();
    if core1.n() != core2.n() {
        throw_r_error("graph size mismatch");
    }
    if names1.len() as u32 != core1.n() || names2.len() as u32 != core2.n() {
        throw_r_error("names length must match number of nodes");
    }
    let mut idx2: HashMap<String, u32> = HashMap::with_capacity(names2.len());
    for (i, s) in names2.iter().enumerate() {
        let k = s.as_str().to_string();
        if idx2.insert(k, i as u32).is_some() {
            throw_r_error("duplicate node name in names2");
        }
    }
    let mut perm = Vec::with_capacity(names1.len());
    for s in names1.iter() {
        let key = s.as_str();
        let j = *idx2.get(key).unwrap_or_else(|| {
            throw_r_error(format!("name '{key}' present in names1 but not in names2"))
        });
        perm.push(j);
    }
    let (norm, count) = shd_with_perm(core1, core2, &perm);
    list!(normalized = norm, count = count as i32).into_robj()
}

#[extendr]
pub fn hd_of_ptrs(g1: ExternalPtr<GraphView>, g2: ExternalPtr<GraphView>) -> Robj {
    let (norm, count) = hd(g1.as_ref().core(), g2.as_ref().core());
    list!(normalized = norm, count = count as i32).into_robj()
}

#[cfg(feature = "gadjid")]
fn to_aid_input(view: &GraphView) -> std::result::Result<aid::AidInput<'_>, String> {
    match view {
        GraphView::Dag(d) => Ok(aid::AidInput::Dag(d.as_ref())),
        GraphView::Pdag(p) => Ok(aid::AidInput::Pdag(p.as_ref())),
        _ => Err("expected graph of type DAG or PDAG".into()),
    }
}

#[cfg(feature = "gadjid")]
fn build_inv_from_names(
    names_true: &extendr_api::prelude::Strings,
    names_guess: &extendr_api::prelude::Strings,
) -> std::result::Result<Vec<usize>, String> {
    use std::collections::HashMap;
    let n = names_true.len();
    if n != names_guess.len() {
        return Err("names length must match number of nodes".into());
    }
    let mut idx_guess: HashMap<String, usize> = HashMap::with_capacity(n);
    for (i, s) in names_guess.iter().enumerate() {
        let k = s.as_str().to_string();
        if idx_guess.insert(k, i).is_some() {
            return Err("duplicate node name in names_guess".into());
        }
    }
    let mut perm = Vec::with_capacity(n);
    for s in names_true.iter() {
        let key = s.as_str();
        let j = *idx_guess
            .get(key)
            .ok_or_else(|| format!("name '{key}' present in names_true but not in names_guess"))?;
        perm.push(j);
    }
    let mut inv = vec![0usize; n];
    for (i, &j) in perm.iter().enumerate() {
        inv[j] = i;
    }
    Ok(inv)
}

#[cfg(feature = "gadjid")]
#[extendr]
pub fn ancestor_aid_of_ptrs(
    g_true: ExternalPtr<GraphView>,
    names_true: Strings,
    g_guess: ExternalPtr<GraphView>,
    names_guess: Strings,
) -> Robj {
    let core_t = g_true.as_ref().core();
    let core_g = g_guess.as_ref().core();
    if core_t.n() != core_g.n() {
        throw_r_error("graph size mismatch");
    }
    let inv = build_inv_from_names(&names_true, &names_guess)
        .unwrap_or_else(|e| throw_r_error(e.to_string()));

    let t = to_aid_input(g_true.as_ref()).unwrap_or_else(|e| throw_r_error(e.to_string()));
    let g = to_aid_input(g_guess.as_ref()).unwrap_or_else(|e| throw_r_error(e.to_string()));
    let (score, count) =
        aid::ancestor_aid_align(t, g, &inv).unwrap_or_else(|e| throw_r_error(e.to_string()));
    list!(score = score, count = count as i32).into_robj()
}

#[cfg(feature = "gadjid")]
#[extendr]
pub fn oset_aid_of_ptrs(
    g_true: ExternalPtr<GraphView>,
    names_true: Strings,
    g_guess: ExternalPtr<GraphView>,
    names_guess: Strings,
) -> Robj {
    let core_t = g_true.as_ref().core();
    let core_g = g_guess.as_ref().core();
    if core_t.n() != core_g.n() {
        throw_r_error("graph size mismatch");
    }
    let inv = build_inv_from_names(&names_true, &names_guess)
        .unwrap_or_else(|e| throw_r_error(e.to_string()));

    let t = to_aid_input(g_true.as_ref()).unwrap_or_else(|e| throw_r_error(e.to_string()));
    let g = to_aid_input(g_guess.as_ref()).unwrap_or_else(|e| throw_r_error(e.to_string()));
    let (score, count) =
        aid::oset_aid_align(t, g, &inv).unwrap_or_else(|e| throw_r_error(e.to_string()));
    list!(score = score, count = count as i32).into_robj()
}

#[cfg(feature = "gadjid")]
#[extendr]
pub fn parent_aid_of_ptrs(
    g_true: ExternalPtr<GraphView>,
    names_true: Strings,
    g_guess: ExternalPtr<GraphView>,
    names_guess: Strings,
) -> Robj {
    let core_t = g_true.as_ref().core();
    let core_g = g_guess.as_ref().core();
    if core_t.n() != core_g.n() {
        throw_r_error("graph size mismatch");
    }
    let inv = build_inv_from_names(&names_true, &names_guess)
        .unwrap_or_else(|e| throw_r_error(e.to_string()));

    let t = to_aid_input(g_true.as_ref()).unwrap_or_else(|e| throw_r_error(e.to_string()));
    let g = to_aid_input(g_guess.as_ref()).unwrap_or_else(|e| throw_r_error(e.to_string()));
    let (score, count) =
        aid::parent_aid_align(t, g, &inv).unwrap_or_else(|e| throw_r_error(e.to_string()));
    list!(score = score, count = count as i32).into_robj()
}

extendr_module! {
    mod metrics;
    fn shd_of_ptrs;
    fn hd_of_ptrs;

    fn ancestor_aid_of_ptrs;
    fn oset_aid_of_ptrs;
    fn parent_aid_of_ptrs;
}

