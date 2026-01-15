// SPDX-License-Identifier: MIT
//! M-separation implementation for Ancestral Graphs.
//!
//! M-separation generalizes d-separation to ancestral graphs with directed,
//! bidirected, and undirected edges.
//!
//! A path m-connects s to t given Z if:
//! 1. Every non-collider on the path is outside Z
//! 2. Every collider c on the path is in Z or has a descendant in Z
//!
//! Where:
//! - Colliders are nodes where two arrowheads meet (← →, ← ↔, ↔ →, ↔ ↔)
//! - Non-colliders include chains and undirected edges

use super::Ag;
use crate::graph::alg::bitset;

impl Ag {
    /// Ancestors mask including the seeds themselves.
    /// `mask[v] == true` iff `v ∈ An(seeds) ∪ seeds`.
    #[allow(dead_code)]
    pub(super) fn ancestors_mask(&self, seeds: &[u32]) -> Vec<bool> {
        bitset::ancestors_mask(seeds, |u| self.parents_of(u), self.n())
    }

    /// Anteriors mask including the seeds themselves.
    /// `mask[v] == true` iff `v ∈ Ant(seeds) ∪ seeds`.
    /// Anteriors are nodes reachable via undirected edges or directed edges pointing toward.
    pub(super) fn anteriors_mask(&self, seeds: &[u32]) -> Vec<bool> {
        let n = self.n() as usize;
        let mut mask = vec![false; n];

        // Start with seeds
        let mut stack: Vec<u32> = seeds.to_vec();
        for &s in seeds {
            mask[s as usize] = true;
        }

        // BFS/DFS to find all anteriors
        while let Some(u) = stack.pop() {
            // Parents (directed edges pointing in)
            for &p in self.parents_of(u) {
                if !mask[p as usize] {
                    mask[p as usize] = true;
                    stack.push(p);
                }
            }
            // Undirected neighbors
            for &w in self.undirected_of(u) {
                if !mask[w as usize] {
                    mask[w as usize] = true;
                    stack.push(w);
                }
            }
        }

        mask
    }

    /// Build a moral adjacency for m-separation in an ancestral graph.
    ///
    /// This extends the standard moralization to handle all three edge types:
    /// 1. Connect parents with children (directed edges become undirected)
    /// 2. Marry parents of common children
    /// 3. Add bidirected edges as undirected edges (spouses connect)
    /// 4. Keep undirected edges as-is
    fn moral_adj_ag(&self, mask: &[bool]) -> Vec<Vec<u32>> {
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

            // Keep undirected edges (they are already symmetric)
            for &u in self.undirected_of(v) {
                if mask[u as usize] {
                    adj[v as usize].push(u);
                    // Note: the reverse will be added when we process node u
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

    /// M-separation test for ancestral graphs.
    ///
    /// Tests whether `xs` is m-separated from `ys` given `z` in the ancestral graph.
    ///
    /// M-separation generalizes d-separation to ancestral graphs by:
    /// 1. Taking the anterior subgraph of `xs ∪ ys ∪ z` (anteriors include nodes
    ///    reachable via undirected edges, not just directed ancestors)
    /// 2. Moralizing it (marrying parents, keeping bidirected and undirected edges)
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

        // Get anterior mask (includes undirected paths)
        let mask = self.anteriors_mask(&seeds);

        // Build moral adjacency
        let adj = self.moral_adj_ag(&mask);

        // Block conditioned nodes
        let mut blocked = vec![false; self.n() as usize];
        for &v in z {
            blocked[v as usize] = true;
        }

        // Check if xs can reach ys in the moral graph
        !Self::reachable_in_moral(&adj, &mask, xs, &blocked, ys)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;
    use std::sync::Arc;

    fn setup() -> (EdgeRegistry, u8, u8, u8) {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let bid = reg.code_of("<->").unwrap();
        let und = reg.code_of("---").unwrap();
        (reg, dir, bid, und)
    }

    #[test]
    fn msep_chain() {
        let (reg, dir, _bid, _und) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Chain: 0 -> 1 -> 2
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();

        let ag = Ag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // 0 and 2 are not m-separated unconditionally
        assert!(!ag.m_separated(&[0], &[2], &[]));

        // 0 and 2 are m-separated given 1
        assert!(ag.m_separated(&[0], &[2], &[1]));
    }

    #[test]
    fn msep_collider() {
        let (reg, dir, _bid, _und) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Collider: 0 -> 2 <- 1
        b.add_edge(0, 2, dir).unwrap();
        b.add_edge(1, 2, dir).unwrap();

        let ag = Ag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // 0 and 1 are m-separated unconditionally (collider blocks path)
        assert!(ag.m_separated(&[0], &[1], &[]));

        // 0 and 1 are NOT m-separated given 2 (collider opens)
        assert!(!ag.m_separated(&[0], &[1], &[2]));
    }

    #[test]
    fn msep_bidirected_confounding() {
        let (reg, dir, bid, _und) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // Valid AG: 0 -> 1, 2 <-> 3, 0 -> 3
        // No anterior constraint violations (bidirected between nodes with no directed path)
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(0, 3, dir).unwrap();
        b.add_edge(2, 3, bid).unwrap();

        let ag = Ag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // 2 and 3 are not m-separated (bidirected edge connects them)
        assert!(!ag.m_separated(&[2], &[3], &[]));

        // 0 and 2 are m-separated (no path)
        assert!(ag.m_separated(&[0], &[2], &[]));
    }

    #[test]
    fn msep_undirected_path() {
        let (reg, _dir, _bid, und) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Undirected chain: 0 --- 1 --- 2
        b.add_edge(0, 1, und).unwrap();
        b.add_edge(1, 2, und).unwrap();

        let ag = Ag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // 0 and 2 are not m-separated unconditionally (connected via undirected)
        assert!(!ag.m_separated(&[0], &[2], &[]));

        // 0 and 2 are m-separated given 1
        assert!(ag.m_separated(&[0], &[2], &[1]));
    }

    #[test]
    fn msep_empty_sets() {
        let (reg, dir, _bid, _und) = setup();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, dir).unwrap();

        let ag = Ag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // Empty X or Y is trivially m-separated
        assert!(ag.m_separated(&[], &[1], &[]));
        assert!(ag.m_separated(&[0], &[], &[]));
    }

    #[test]
    fn msep_m_bias() {
        let (reg, dir, _bid, _und) = setup();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        // M-bias structure:
        // U1 -> X, U1 -> M, U2 -> M, U2 -> Y
        // 0:U1, 1:U2, 2:X, 3:M, 4:Y
        b.add_edge(0, 2, dir).unwrap(); // U1 -> X
        b.add_edge(0, 3, dir).unwrap(); // U1 -> M
        b.add_edge(1, 3, dir).unwrap(); // U2 -> M
        b.add_edge(1, 4, dir).unwrap(); // U2 -> Y

        let ag = Ag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // X and Y are m-separated unconditionally
        assert!(ag.m_separated(&[2], &[4], &[]));

        // X and Y are NOT m-separated given M (collider bias)
        assert!(!ag.m_separated(&[2], &[4], &[3]));
    }

    #[test]
    fn msep_pure_bidirected_chain() {
        let (reg, _dir, bid, _und) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // Chain of bidirected: 0 <-> 1 <-> 2
        b.add_edge(0, 1, bid).unwrap();
        b.add_edge(1, 2, bid).unwrap();

        let ag = Ag::new(Arc::new(b.finalize().unwrap())).unwrap();

        // M-separation uses the ancestral subgraph (through directed edges only).
        // Since there are no directed edges, An({0,2}) = {0,2} (node 1 is not included).
        // In this subgraph, 0 and 2 have no connecting edges, so they are m-separated.
        assert!(ag.m_separated(&[0], &[2], &[]));

        // When conditioning on 1, the ancestral subgraph includes An({0,1,2}) = {0,1,2}.
        // In this subgraph, the bidirected edges create moral edges 0-1 and 1-2.
        // But node 1 is blocked, so 0 and 2 are still m-separated.
        assert!(ag.m_separated(&[0], &[2], &[1]));
    }
}
