// SPDX-License-Identifier: MIT
//! Moral graph construction utilities.

/// Moralized adjacency within mask. Undirected edges among ancestors.
/// Output adjacency lists are sorted and deduplicated.
///
/// # Arguments
/// * `n` - Total number of nodes
/// * `parents_of` - Function returning parents of a given node
/// * `mask` - Boolean mask indicating which nodes to include
pub fn moral_adj<'a, F>(n: u32, parents_of: F, mask: &[bool]) -> Vec<Vec<u32>>
where
    F: Fn(u32) -> &'a [u32],
{
    let n = n as usize;
    let mut adj = vec![Vec::<u32>::new(); n];
    for v in 0..n as u32 {
        if !mask[v as usize] {
            continue;
        }
        let pa = parents_of(v);
        // Connect child with each included parent.
        for &p in pa {
            if mask[p as usize] {
                adj[v as usize].push(p);
                adj[p as usize].push(v);
            }
        }
        // Marry included parents.
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
    }
    // Dedup neighbors per node.
    for v in &mut adj {
        v.sort_unstable();
        v.dedup();
    }
    adj
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn moral_adj_simple_chain() {
        // Graph: 0 -> 1 -> 2
        let parents_of = |n: u32| -> &[u32] {
            match n {
                1 => &[0],
                2 => &[1],
                _ => &[],
            }
        };

        let mask = vec![true, true, true];
        let adj = moral_adj(3, parents_of, &mask);

        // Moralized: 0-1, 1-2
        assert_eq!(adj[0], vec![1]);
        assert_eq!(adj[1], vec![0, 2]);
        assert_eq!(adj[2], vec![1]);
    }

    #[test]
    fn moral_adj_v_structure() {
        // Graph: 0 -> 2, 1 -> 2 (V-structure)
        let parents_of = |n: u32| -> &[u32] {
            match n {
                2 => &[0, 1],
                _ => &[],
            }
        };

        let mask = vec![true, true, true];
        let adj = moral_adj(3, parents_of, &mask);

        // Moralized: 0-2, 1-2, and 0-1 (married parents)
        assert_eq!(adj[0], vec![1, 2]);
        assert_eq!(adj[1], vec![0, 2]);
        assert_eq!(adj[2], vec![0, 1]);
    }

    #[test]
    fn moral_adj_with_mask() {
        // Graph: 0 -> 2 <- 1, but exclude node 1
        let parents_of = |n: u32| -> &[u32] {
            match n {
                2 => &[0, 1],
                _ => &[],
            }
        };

        let mask = vec![true, false, true];
        let adj = moral_adj(3, parents_of, &mask);

        // Only 0-2 edge, no connection to 1
        assert_eq!(adj[0], vec![2]);
        assert!(adj[1].is_empty());
        assert_eq!(adj[2], vec![0]);
    }

    #[test]
    fn moral_adj_no_parents() {
        // Graph: isolated nodes
        let parents_of = |_n: u32| -> &[u32] { &[] };

        let mask = vec![true, true];
        let adj = moral_adj(2, parents_of, &mask);

        assert!(adj[0].is_empty());
        assert!(adj[1].is_empty());
    }
}
