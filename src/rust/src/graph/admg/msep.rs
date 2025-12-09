// SPDX-License-Identifier: MIT
//! M-separation implementation for ADMGs.

use super::Admg;
use crate::graph::alg::bitset;

impl Admg {
    /// Ancestors mask including the seeds themselves.
    /// `mask[v] == true` iff `v ∈ An(seeds) ∪ seeds`.
    pub(super) fn ancestors_mask(&self, seeds: &[u32]) -> Vec<bool> {
        bitset::ancestors_mask(seeds, |u| self.parents_of(u), self.n())
    }

    /// Build a moral adjacency for m-separation in an ADMG.
    ///
    /// This extends the standard moralization to handle bidirected edges:
    /// 1. Connect parents with children (undirected)
    /// 2. Marry parents of common children
    /// 3. Add bidirected edges as undirected edges
    fn moral_adj_admg(&self, mask: &[bool]) -> Vec<Vec<u32>> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];

        for v in 0..n as u32 {
            if !mask[v as usize] {
                continue;
            }

            // Connect with parents (as in standard moralization)
            let pa = self.parents_of(v);
            for &p in pa {
                if mask[p as usize] {
                    adj[v as usize].push(p);
                    adj[p as usize].push(v);
                }
            }

            // Marry parents of common children
            for i in 0..pa.len() {
                let pi = pa[i] as usize;
                if !mask[pi] {
                    continue;
                }
                for j in i + 1..pa.len() {
                    let pj = pa[j] as usize;
                    if !mask[pj] {
                        continue;
                    }
                    adj[pi].push(pa[j]);
                    adj[pj].push(pa[i]);
                }
            }

            // Add bidirected edges as undirected (spouses connect in moral graph)
            for &s in self.spouses_of(v) {
                if mask[s as usize] {
                    adj[v as usize].push(s);
                    // Note: the reverse will be added when we process node s
                }
            }
        }

        // Dedup neighbors per node
        for v in &mut adj {
            v.sort_unstable();
            v.dedup();
        }
        adj
    }

    /// BFS reachability check over moral graph.
    /// Returns true if any node in `src` can reach any node in `tgt` while avoiding `blocked`.
    fn reachable_in_moral(
        adj: &[Vec<u32>],
        mask: &[bool],
        src: &[u32],
        blocked: &[bool],
        tgt: &[u32],
    ) -> bool {
        use std::collections::VecDeque;
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

    /// M-separation test for ADMGs.
    ///
    /// Tests whether `xs` is m-separated from `ys` given `z` in the ADMG.
    ///
    /// M-separation generalizes d-separation to ADMGs by:
    /// 1. Taking the ancestral subgraph of `xs ∪ ys ∪ z`
    /// 2. Moralizing it (marrying parents, keeping bidirected edges)
    /// 3. Removing conditioning nodes
    /// 4. Testing path connectivity
    ///
    /// Returns `true` iff `xs ⊥_m ys | z`.
    pub fn m_separated(&self, xs: &[u32], ys: &[u32], z: &[u32]) -> bool {
        if xs.is_empty() || ys.is_empty() {
            return true;
        }

        // Collect all seeds
        let mut seeds = xs.to_vec();
        seeds.extend_from_slice(ys);
        seeds.extend_from_slice(z);
        seeds.sort_unstable();
        seeds.dedup();

        // Get ancestral mask
        let mask = self.ancestors_mask(&seeds);

        // Build moral adjacency
        let adj = self.moral_adj_admg(&mask);

        // Block conditioned nodes
        let mut blocked = vec![false; self.n() as usize];
        for &v in z {
            blocked[v as usize] = true;
        }

        // Check if xs can reach ys in the moral graph
        !Self::reachable_in_moral(&adj, &mask, xs, &blocked, ys)
    }
}
