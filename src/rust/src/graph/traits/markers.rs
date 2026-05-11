// SPDX-License-Identifier: MIT
//! Marker traits for graph invariants.
//!
//! Each marker is type-level evidence that an invariant was checked at
//! construction. Marker traits have no methods and no runtime cost.

/// The directed sub-skeleton has no directed cycle.
///
/// Implementors guarantee that `directed_part_is_acyclic` on their CSR core
/// would return `true`. Algorithms that need an acyclicity check should accept
/// `&impl Acyclic` to skip the runtime probe.
pub trait Acyclic {}

/// A PDAG-shaped graph for which Meek's orientation rules R1–R4 have reached
/// a fixed point. Equivalent to an MPDAG.
pub trait MeekClosed {}

/// The graph has no undirected (`---`) edges.
pub trait NoUndirected {}

/// The graph has no bidirected (`<->`) edges.
pub trait NoBidirected {}

/// The graph has only directed (`-->`) edges. Conjunction of `NoUndirected`
/// and `NoBidirected`, automatically implemented for any type that has both.
pub trait OnlyDirected: NoUndirected + NoBidirected {}
impl<T: NoUndirected + NoBidirected> OnlyDirected for T {}
