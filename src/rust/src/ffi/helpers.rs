// SPDX-License-Identifier: MIT
//! Helper functions for FFI conversions.

use extendr_api::prelude::*;

/// Convert Rint to u32, throwing R error on NA or negative.
pub fn rint_to_u32(x: Rint, field: &str) -> u32 {
    if x.is_na() {
        throw_r_error(format!("NA in `{}`", field));
    }
    let v = x.inner();
    if v < 0 {
        throw_r_error(format!(
            "`{}` must be >= 0. Note that the input number from R might have been subtracted with 1.",
            field
        ));
    }
    v as u32
}

/// Convert Rint to u8, throwing R error on NA or out of range.
pub fn rint_to_u8(x: Rint, field: &str) -> u8 {
    if x.is_na() {
        throw_r_error(format!("NA in `{}`", field));
    }
    let v = x.inner();
    if !(0..=255).contains(&v) {
        throw_r_error(format!("`{}` must be in 0..=255", field));
    }
    v as u8
}

/// Convert Rbool to bool, throwing R error on NA.
pub fn rbool_to_bool(x: Rbool, field: &str) -> bool {
    if x.is_na() {
        throw_r_error(format!("NA in `{}`", field));
    }
    x.is_true()
}

