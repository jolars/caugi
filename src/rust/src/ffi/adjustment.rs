// SPDX-License-Identifier: MIT
//! R bindings for adjustment and separation operations.

use extendr_api::prelude::*;

use super::helpers::{rbool_to_bool, rint_to_u32};
use crate::graph::view::GraphView;

#[extendr]
pub fn d_separated_ptr(g: ExternalPtr<GraphView>, xs: Integers, ys: Integers, z: Integers) -> bool {
    let xs_u: Vec<u32> = xs.iter().map(|ri| rint_to_u32(ri, "xs")).collect();
    let ys_u: Vec<u32> = ys.iter().map(|ri| rint_to_u32(ri, "ys")).collect();
    let z_u: Vec<u32> = z.iter().map(|ri| rint_to_u32(ri, "z")).collect();
    for &i in xs_u.iter().chain(ys_u.iter()).chain(z_u.iter()) {
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i + 1));
        }
    }
    g.as_ref()
        .d_separated(&xs_u, &ys_u, &z_u)
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn m_separated_ptr(g: ExternalPtr<GraphView>, xs: Integers, ys: Integers, z: Integers) -> bool {
    let xs_u: Vec<u32> = xs.iter().map(|ri| rint_to_u32(ri, "xs")).collect();
    let ys_u: Vec<u32> = ys.iter().map(|ri| rint_to_u32(ri, "ys")).collect();
    let z_u: Vec<u32> = z.iter().map(|ri| rint_to_u32(ri, "z")).collect();
    for &i in xs_u.iter().chain(ys_u.iter()).chain(z_u.iter()) {
        if i >= g.as_ref().n() {
            throw_r_error(format!("Index {} is out of bounds", i + 1));
        }
    }
    g.as_ref()
        .m_separated(&xs_u, &ys_u, &z_u)
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn adjustment_set_parents_ptr(g: ExternalPtr<GraphView>, xs: Integers, ys: Integers) -> Robj {
    let xs_u: Vec<u32> = xs.iter().map(|ri| rint_to_u32(ri, "xs")).collect();
    let ys_u: Vec<u32> = ys.iter().map(|ri| rint_to_u32(ri, "ys")).collect();
    g.as_ref()
        .adjustment_set_parents(&xs_u, &ys_u)
        .map(|v| v.into_iter().map(|x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn adjustment_set_backdoor_ptr(g: ExternalPtr<GraphView>, xs: Integers, ys: Integers) -> Robj {
    let xs_u: Vec<u32> = xs.iter().map(|ri| rint_to_u32(ri, "xs")).collect();
    let ys_u: Vec<u32> = ys.iter().map(|ri| rint_to_u32(ri, "ys")).collect();
    g.as_ref()
        .adjustment_set_backdoor(&xs_u, &ys_u)
        .map(|v| v.into_iter().map(|x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn adjustment_set_optimal_ptr(g: ExternalPtr<GraphView>, x: i32, y: i32) -> Robj {
    if x < 0 || y < 0 {
        throw_r_error("x and y must be >= 0");
    }
    g.as_ref()
        .adjustment_set_optimal(x as u32, y as u32)
        .map(|v| v.into_iter().map(|x| x as i32).collect_robj())
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn is_valid_backdoor_set_ptr(g: ExternalPtr<GraphView>, x: i32, y: i32, z: Integers) -> bool {
    if x < 0 || y < 0 {
        throw_r_error("x and y must be >= 0");
    }
    let z_u: Vec<u32> = z.iter().map(|ri| rint_to_u32(ri, "z")).collect();
    g.as_ref()
        .is_valid_backdoor_set(x as u32, y as u32, &z_u)
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn all_backdoor_sets_ptr(
    g: ExternalPtr<GraphView>,
    x: i32,
    y: i32,
    minimal: Rbool,
    max_size: i32,
) -> Robj {
    if x < 0 || y < 0 {
        throw_r_error("x and y must be >= 0");
    }
    let max_size = rint_to_u32(Rint::from(max_size), "max_size");
    let sets = g
        .as_ref()
        .all_backdoor_sets(
            x as u32,
            y as u32,
            rbool_to_bool(minimal, "minimal"),
            max_size,
        )
        .unwrap_or_else(|e| throw_r_error(e));
    let robjs: Vec<Robj> = sets
        .into_iter()
        .map(|v| v.into_iter().map(|u| u as i32).collect_robj())
        .collect();
    extendr_api::prelude::List::from_values(robjs).into_robj()
}

#[extendr]
pub fn is_valid_adjustment_set_admg_ptr(
    g: ExternalPtr<GraphView>,
    xs: Integers,
    ys: Integers,
    z: Integers,
) -> bool {
    let xs_u: Vec<u32> = xs.iter().map(|ri| rint_to_u32(ri, "xs")).collect();
    let ys_u: Vec<u32> = ys.iter().map(|ri| rint_to_u32(ri, "ys")).collect();
    let z_u: Vec<u32> = z.iter().map(|ri| rint_to_u32(ri, "z")).collect();
    g.as_ref()
        .is_valid_adjustment_set_admg(&xs_u, &ys_u, &z_u)
        .unwrap_or_else(|e| throw_r_error(e))
}

#[extendr]
pub fn all_adjustment_sets_admg_ptr(
    g: ExternalPtr<GraphView>,
    xs: Integers,
    ys: Integers,
    minimal: Rbool,
    max_size: i32,
) -> Robj {
    let xs_u: Vec<u32> = xs.iter().map(|ri| rint_to_u32(ri, "xs")).collect();
    let ys_u: Vec<u32> = ys.iter().map(|ri| rint_to_u32(ri, "ys")).collect();
    let max_size = rint_to_u32(Rint::from(max_size), "max_size");
    let sets = g
        .as_ref()
        .all_adjustment_sets_admg(&xs_u, &ys_u, rbool_to_bool(minimal, "minimal"), max_size)
        .unwrap_or_else(|e| throw_r_error(e));
    let robjs: Vec<Robj> = sets
        .into_iter()
        .map(|v| v.into_iter().map(|u| u as i32).collect_robj())
        .collect();
    extendr_api::prelude::List::from_values(robjs).into_robj()
}

extendr_module! {
    mod adjustment;
    fn d_separated_ptr;
    fn m_separated_ptr;
    fn adjustment_set_parents_ptr;
    fn adjustment_set_backdoor_ptr;
    fn adjustment_set_optimal_ptr;
    fn is_valid_backdoor_set_ptr;
    fn all_backdoor_sets_ptr;
    fn is_valid_adjustment_set_admg_ptr;
    fn all_adjustment_sets_admg_ptr;
}

