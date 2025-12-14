// SPDX-License-Identifier: MIT
//! FFI bindings for R interoperability.
//!
//! This module contains all extendr-annotated functions that are exposed to R.
//! The functions are organized by domain:
//!
//! - `registry`: Edge registry operations
//! - `builder`: Graph builder operations
//! - `queries`: Graph query operations (parents, children, etc.)
//! - `adjustment`: Adjustment and separation operations
//! - `transforms`: Graph transformation operations (skeleton, moralize, etc.)
//! - `metrics`: Graph metrics (SHD, HD, AID)

mod helpers;

pub mod adjustment;
pub mod builder;
pub mod metrics;
pub mod queries;
pub mod registry;
pub mod transforms;

