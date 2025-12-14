// SPDX-License-Identifier: MIT
//! Generic graph traversal algorithms.
//!
//! These functions provide reusable traversal patterns that work with any graph type
//! by accepting closures for accessing neighbors.

use super::bitset;

/// Compute ancestors of a single node using iterative DFS.
///
/// # Arguments
/// * `n` - Total number of nodes in the graph
/// * `i` - The node to find ancestors of
/// * `parents_of` - Function returning parents of a given node
///
/// Returns a sorted vector of ancestor node indices (not including `i` itself).
pub fn ancestors_of<'a, F>(n: u32, i: u32, parents_of: F) -> Vec<u32>
where
    F: Fn(u32) -> &'a [u32],
{
    let n = n as usize;
    let mut seen = vec![false; n];
    let mut stack: Vec<u32> = parents_of(i).to_vec();
    while let Some(u) = stack.pop() {
        let ui = u as usize;
        if std::mem::replace(&mut seen[ui], true) {
            continue;
        }
        stack.extend_from_slice(parents_of(u));
    }
    bitset::collect_from_mask(&seen)
}

/// Compute descendants of a single node using iterative DFS.
///
/// # Arguments
/// * `n` - Total number of nodes in the graph
/// * `i` - The node to find descendants of
/// * `children_of` - Function returning children of a given node
///
/// Returns a sorted vector of descendant node indices (not including `i` itself).
pub fn descendants_of<'a, F>(n: u32, i: u32, children_of: F) -> Vec<u32>
where
    F: Fn(u32) -> &'a [u32],
{
    let n = n as usize;
    let mut seen = vec![false; n];
    let mut stack: Vec<u32> = children_of(i).to_vec();
    while let Some(u) = stack.pop() {
        let ui = u as usize;
        if std::mem::replace(&mut seen[ui], true) {
            continue;
        }
        stack.extend_from_slice(children_of(u));
    }
    bitset::collect_from_mask(&seen)
}

/// Compute DAG-style Markov blanket of a single node.
///
/// The Markov blanket consists of: Pa(i) ∪ Ch(i) ∪ (⋃ Pa(c) \ {i} : c∈Ch(i))
///
/// # Arguments
/// * `n` - Total number of nodes in the graph
/// * `i` - The node to find the Markov blanket of
/// * `parents_of` - Function returning parents of a given node
/// * `children_of` - Function returning children of a given node
///
/// Returns a sorted vector of node indices in the Markov blanket (not including `i` itself).
pub fn markov_blanket_dag<'a, F, G>(n: u32, i: u32, parents_of: F, children_of: G) -> Vec<u32>
where
    F: Fn(u32) -> &'a [u32],
    G: Fn(u32) -> &'a [u32],
{
    let n = n as usize;
    let mut m = vec![false; n];

    // Parents
    for &p in parents_of(i) {
        m[p as usize] = true;
    }

    // Children
    for &c in children_of(i) {
        m[c as usize] = true;
        // Co-parents of children
        for &p in parents_of(c) {
            if p != i {
                m[p as usize] = true;
            }
        }
    }

    m[i as usize] = false; // exclude self
    bitset::collect_from_mask(&m)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ancestors_of_simple_chain() {
        // Graph: 2 -> 1 -> 0
        let parents_of = |n: u32| -> &[u32] {
            match n {
                0 => &[1],
                1 => &[2],
                _ => &[],
            }
        };

        assert_eq!(ancestors_of(3, 0, parents_of), vec![1, 2]);
        assert_eq!(ancestors_of(3, 1, parents_of), vec![2]);
        assert_eq!(ancestors_of(3, 2, parents_of), Vec::<u32>::new());
    }

    #[test]
    fn descendants_of_simple_chain() {
        // Graph: 0 -> 1 -> 2
        let children_of = |n: u32| -> &[u32] {
            match n {
                0 => &[1],
                1 => &[2],
                _ => &[],
            }
        };

        assert_eq!(descendants_of(3, 0, children_of), vec![1, 2]);
        assert_eq!(descendants_of(3, 1, children_of), vec![2]);
        assert_eq!(descendants_of(3, 2, children_of), Vec::<u32>::new());
    }

    #[test]
    fn markov_blanket_dag_v_structure() {
        // Graph: 2 -> 1 <- 0 -> 3
        let p1 = [0, 2];
        let parents_of = |n: u32| -> &[u32] {
            match n {
                1 => &p1,
                3 => &[0],
                _ => &[],
            }
        };
        let c0 = [1, 3];
        let children_of = |n: u32| -> &[u32] {
            match n {
                0 => &c0,
                2 => &[1],
                _ => &[],
            }
        };

        // MB(0) = pa(0)=∅, ch(0)={1,3}, spouses via 1={2}
        assert_eq!(markov_blanket_dag(4, 0, parents_of, children_of), vec![1, 2, 3]);
    }
}

