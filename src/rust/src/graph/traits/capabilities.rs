// SPDX-License-Identifier: MIT
//! Capability traits exposing shared accessors across class wrappers.

/// Sorted slice access to the directed neighbors of every node.
///
/// Class wrappers that carry a directed sub-graph (`Dag`, `Pdag`, `Mpdag`,
/// `Admg`, `Ag`) all implement this. Algorithms that only need parents and
/// children can be written generically over `impl DirectedNeighbors`.
pub trait DirectedNeighbors {
    fn n(&self) -> u32;
    fn parents_of(&self, i: u32) -> &[u32];
    fn children_of(&self, i: u32) -> &[u32];
}
