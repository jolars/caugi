// SPDX-License-Identifier: MIT
//! Districts (c-components) implementation for ADMGs.

use super::Admg;
use crate::graph::alg::bitset;

impl Admg {
    /// Compute all districts (c-components) of the ADMG.
    ///
    /// A district is a maximal set of nodes connected via bidirected edges.
    /// Returns a vector of districts, each district being a sorted vector of node indices.
    pub fn districts(&self) -> Vec<Vec<u32>> {
        let n = self.n() as usize;
        let mut comp = vec![usize::MAX; n];
        let mut stack = Vec::new();
        let mut cid = 0usize;

        for s in 0..n {
            if comp[s] != usize::MAX {
                continue;
            }
            // Start a new district
            comp[s] = cid;
            stack.clear();
            stack.push(s as u32);

            // BFS/DFS over bidirected edges
            while let Some(u) = stack.pop() {
                for &w in self.spouses_of(u) {
                    let wi = w as usize;
                    if comp[wi] == usize::MAX {
                        comp[wi] = cid;
                        stack.push(w);
                    }
                }
            }
            cid += 1;
        }

        // Collect nodes into districts
        let mut districts: Vec<Vec<u32>> = vec![Vec::new(); cid];
        for (node, &c) in comp.iter().enumerate() {
            districts[c].push(node as u32);
        }

        // Sort each district for determinism
        for d in &mut districts {
            d.sort_unstable();
        }

        districts
    }

    /// Get the district (c-component) containing node `i`.
    ///
    /// Returns a sorted vector of all nodes in the same district as `i`.
    pub fn district_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut stack = vec![i];
        seen[i as usize] = true;

        while let Some(u) = stack.pop() {
            for &w in self.spouses_of(u) {
                let wi = w as usize;
                if !seen[wi] {
                    seen[wi] = true;
                    stack.push(w);
                }
            }
        }

        bitset::collect_from_mask(&seen)
    }

    /// Get the district assignment for all nodes.
    ///
    /// Returns a vector where `result[i]` is the district index of node `i`.
    pub fn district_membership(&self) -> Vec<usize> {
        let n = self.n() as usize;
        let mut comp = vec![usize::MAX; n];
        let mut stack = Vec::new();
        let mut cid = 0usize;

        for s in 0..n {
            if comp[s] != usize::MAX {
                continue;
            }
            comp[s] = cid;
            stack.clear();
            stack.push(s as u32);

            while let Some(u) = stack.pop() {
                for &w in self.spouses_of(u) {
                    let wi = w as usize;
                    if comp[wi] == usize::MAX {
                        comp[wi] = cid;
                        stack.push(w);
                    }
                }
            }
            cid += 1;
        }

        comp
    }

    /// Number of districts (c-components) in the ADMG.
    ///
    /// Uses Union-Find for O(n α(n)) complexity without allocating full district lists.
    pub fn num_districts(&self) -> usize {
        let n = self.n() as usize;
        if n == 0 {
            return 0;
        }

        let mut count = n; // Start with each node as its own component
        let mut parent: Vec<usize> = (0..n).collect();

        // Union-Find with path compression
        fn find(parent: &mut [usize], mut x: usize) -> usize {
            while parent[x] != x {
                parent[x] = parent[parent[x]]; // Path compression
                x = parent[x];
            }
            x
        }

        for v in 0..n as u32 {
            for &s in self.spouses_of(v) {
                let pv = find(&mut parent, v as usize);
                let ps = find(&mut parent, s as usize);
                if pv != ps {
                    parent[pv] = ps;
                    count -= 1;
                }
            }
        }
        count
    }
}
