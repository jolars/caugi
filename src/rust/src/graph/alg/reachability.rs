// SPDX-License-Identifier: MIT
//! Reachability algorithms for graphs.

use std::collections::VecDeque;

/// BFS over an adjacency graph to check reachability from `src` to any `tgt`
/// while ignoring `blocked` nodes. Only visits nodes inside `mask`.
///
/// # Arguments
/// * `adj` - Adjacency lists for each node
/// * `mask` - Boolean mask indicating which nodes to include
/// * `src` - Source nodes to start from
/// * `blocked` - Nodes that block the path
/// * `tgt` - Target nodes to reach
///
/// Returns `true` if any source can reach any target without going through blocked nodes.
pub fn reachable_to_any(
    adj: &[Vec<u32>],
    mask: &[bool],
    src: &[u32],
    blocked: &[bool],
    tgt: &[u32],
) -> bool {
    let n = adj.len();
    let mut target = vec![false; n];
    for &y in tgt {
        if !blocked[y as usize] {
            target[y as usize] = true;
        }
    }
    let mut seen = vec![false; n];
    let mut q = VecDeque::new();
    for &x in src {
        let xi = x as usize;
        if mask[xi] && !blocked[xi] && !seen[xi] {
            seen[xi] = true;
            q.push_back(x);
        }
    }
    while let Some(u) = q.pop_front() {
        let ui = u as usize;
        if target[ui] {
            return true;
        }
        for &w in &adj[ui] {
            let wi = w as usize;
            if mask[wi] && !blocked[wi] && !seen[wi] {
                seen[wi] = true;
                q.push_back(w);
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reachable_to_any_simple_path() {
        // 0 - 1 - 2
        let adj = vec![vec![1], vec![0, 2], vec![1]];
        let mask = vec![true, true, true];
        let blocked = vec![false, false, false];

        assert!(reachable_to_any(&adj, &mask, &[0], &blocked, &[2]));
    }

    #[test]
    fn reachable_to_any_blocked() {
        // 0 - 1 - 2, block node 1
        let adj = vec![vec![1], vec![0, 2], vec![1]];
        let mask = vec![true, true, true];
        let blocked = vec![false, true, false];

        assert!(!reachable_to_any(&adj, &mask, &[0], &blocked, &[2]));
    }

    #[test]
    fn reachable_to_any_outside_mask() {
        // 0 - 1 - 2, node 1 not in mask
        let adj = vec![vec![1], vec![0, 2], vec![1]];
        let mask = vec![true, false, true];
        let blocked = vec![false, false, false];

        assert!(!reachable_to_any(&adj, &mask, &[0], &blocked, &[2]));
    }
}
