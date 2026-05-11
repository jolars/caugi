// SPDX-License-Identifier: MIT
//! Marker and capability traits for graph classes.
//!
//! Marker traits (`Acyclic`, `MeekClosed`, `NoUndirected`, `NoBidirected`,
//! `OnlyDirected`) encode invariants that have already been validated at
//! construction time. Algorithms generic over these traits can skip the
//! corresponding runtime probe.
//!
//! Capability traits (`DirectedNeighbors`) expose the minimal accessor surface
//! shared across class wrappers, so algorithms can be written generically.

pub mod capabilities;
pub mod markers;

pub use capabilities::DirectedNeighbors;
pub use markers::{Acyclic, MeekClosed, NoBidirected, NoUndirected, OnlyDirected};
