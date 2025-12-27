// SPDX-License-Identifier: MIT
//! Graph transformations for DAGs: skeleton, moralize, to_cpdag, latent_project.

use super::Dag;
use crate::edges::EdgeClass;
use crate::graph::admg::Admg;
use crate::graph::alg::csr;
use crate::graph::pdag::Pdag;
use crate::graph::ug::Ug;
use crate::graph::CaugiGraph;
use std::collections::{HashSet, VecDeque};
use std::sync::Arc;

impl Dag {
    /// Build the moral graph of the entire DAG as an undirected graph core.
    pub fn moralize_core(&self) -> Result<CaugiGraph, String> {
        let mask = vec![true; self.n() as usize];
        let adj = self.moral_adj(&mask);
        csr::build_ug_core_from_adj(self.core_ref(), &adj)
    }

    /// Build the skeleton (undirected version) of the DAG as an undirected graph core.
    pub fn skeleton_core(&self) -> Result<CaugiGraph, String> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];
        for u in 0..self.n() {
            for &v in self.children_of(u) {
                adj[u as usize].push(v);
                adj[v as usize].push(u);
            }
        }
        for v in &mut adj {
            v.sort_unstable();
            v.dedup();
        }
        csr::build_ug_core_from_adj(self.core_ref(), &adj)
    }

    /// UG skeleton: undirect every edge in the DAG.
    pub fn skeleton(&self) -> Result<Ug, String> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];
        for i in 0..n {
            let mut v = self.neighbors_of(i as u32).to_vec();
            v.sort_unstable();
            v.dedup();
            adj[i] = v;
        }
        let core = csr::build_ug_core_from_adj(self.core_ref(), &adj)?;
        Ug::new(Arc::new(core))
    }

    /// UG moral graph: undirect all edges and marry parents.
    pub fn moralize(&self) -> Result<Ug, String> {
        let mask = vec![true; self.n() as usize];
        let adj = self.moral_adj(&mask);
        let core = csr::build_ug_core_from_adj(self.core_ref(), &adj)?;
        Ug::new(Arc::new(core))
    }

    /// Project out latent variables from a DAG to produce an ADMG.
    ///
    /// For each pair of observed nodes (X, Y) **without an existing directed edge**,
    /// adds a bidirected edge X <-> Y if they share a latent ancestor
    /// (i.e., An(X) ∩ An(Y) ∩ Latents is non-empty).
    /// Directed edges between observed nodes are preserved.
    ///
    /// Note: Since simple graphs cannot have parallel edges, a bidirected edge is
    /// NOT added between nodes that already have a directed edge. The directed edge
    /// takes precedence.
    ///
    /// # Arguments
    /// * `latents` - Slice of node indices to project out (0-indexed)
    ///
    /// # Returns
    /// An `Admg` containing only the observed (non-latent) nodes with:
    /// - Directed edges preserved from the original DAG
    /// - Bidirected edges added where nodes share latent ancestors and have no direct edge
    ///
    /// # Errors
    /// Returns an error if any latent index is out of bounds.
    pub fn latent_project(&self, latents: &[u32]) -> Result<Admg, String> {
        let n = self.n() as usize;

        // Validate latent indices
        for &l in latents {
            if l >= self.n() {
                return Err(format!(
                    "Latent index {} is out of bounds (n = {})",
                    l,
                    self.n()
                ));
            }
        }

        // Build latent mask
        let mut is_latent = vec![false; n];
        for &l in latents {
            is_latent[l as usize] = true;
        }

        // Collect observed nodes (preserving order)
        let observed: Vec<u32> = (0..self.n()).filter(|&i| !is_latent[i as usize]).collect();
        let num_observed = observed.len();

        // Find edge codes
        let specs = &self.core_ref().registry.specs;
        let mut dir_code: Option<u8> = None;
        let mut bid_code: Option<u8> = None;
        for (i, s) in specs.iter().enumerate() {
            match s.class {
                EdgeClass::Directed => {
                    if dir_code.is_none() || s.glyph == "-->" {
                        dir_code = Some(i as u8)
                    }
                }
                EdgeClass::Bidirected => {
                    if bid_code.is_none() || s.glyph == "<->" {
                        bid_code = Some(i as u8)
                    }
                }
                _ => {}
            }
        }
        let dir = dir_code.ok_or("No Directed edge spec in registry")?;
        let bid = bid_code.ok_or("No Bidirected edge spec in registry")?;

        if num_observed == 0 {
            // All nodes are latent - return empty ADMG
            let core = CaugiGraph::from_csr(
                vec![0u32],
                vec![],
                vec![],
                vec![],
                true,
                self.core_ref().registry.clone(),
            )?;
            return Admg::new(Arc::new(core));
        }

        // Build mapping from old index to new index (only for observed nodes)
        let mut old_to_new: Vec<Option<u32>> = vec![None; n];
        for (new_idx, &old_idx) in observed.iter().enumerate() {
            old_to_new[old_idx as usize] = Some(new_idx as u32);
        }

        // Pre-compute ancestor sets for each observed node (including latent ancestors)
        let mut ancestors_incl_self: Vec<HashSet<u32>> = Vec::with_capacity(num_observed);
        for &obs in &observed {
            let mut anc_set: HashSet<u32> = self.ancestors_of(obs).into_iter().collect();
            anc_set.insert(obs); // Include self for completeness
            ancestors_incl_self.push(anc_set);
        }

        // Build adjacency lists for the new graph
        // Each node stores: (parents, spouses, children) as sorted vectors
        let mut parents: Vec<Vec<u32>> = vec![Vec::new(); num_observed];
        let mut spouses: Vec<Vec<u32>> = vec![Vec::new(); num_observed];
        let mut children: Vec<Vec<u32>> = vec![Vec::new(); num_observed];

        // Track which pairs already have a directed edge (to avoid parallel edges)
        // has_directed_edge[i][j] means there's a directed edge between node i and node j
        // (in either direction: i->j or j->i)
        let mut has_directed_edge: Vec<HashSet<u32>> = vec![HashSet::new(); num_observed];

        // Add directed edges between observed nodes
        for &u in &observed {
            for &v in self.children_of(u) {
                if !is_latent[v as usize] {
                    // Both u and v are observed
                    let new_u = old_to_new[u as usize].unwrap();
                    let new_v = old_to_new[v as usize].unwrap();
                    children[new_u as usize].push(new_v);
                    parents[new_v as usize].push(new_u);
                    // Mark this pair as having a directed edge
                    has_directed_edge[new_u as usize].insert(new_v);
                    has_directed_edge[new_v as usize].insert(new_u);
                }
            }
        }

        // Add bidirected edges for pairs that share a latent ancestor
        // BUT only if there's no existing directed edge between them
        for i in 0..num_observed {
            for j in (i + 1)..num_observed {
                // Skip if there's already a directed edge between these nodes
                if has_directed_edge[i].contains(&(j as u32)) {
                    continue;
                }

                // Check if An(observed[i]) ∩ An(observed[j]) ∩ Latents is non-empty
                let has_shared_latent_ancestor = ancestors_incl_self[i]
                    .iter()
                    .any(|&a| is_latent[a as usize] && ancestors_incl_self[j].contains(&a));

                if has_shared_latent_ancestor {
                    spouses[i].push(j as u32);
                    spouses[j].push(i as u32);
                }
            }
        }

        // Sort all adjacency lists
        for i in 0..num_observed {
            parents[i].sort_unstable();
            spouses[i].sort_unstable();
            children[i].sort_unstable();
        }

        // Build CSR representation
        let mut row_index = Vec::with_capacity(num_observed + 1);
        row_index.push(0u32);
        for i in 0..num_observed {
            let count = parents[i].len() + spouses[i].len() + children[i].len();
            row_index.push(row_index[i] + count as u32);
        }

        let nnz = *row_index.last().unwrap() as usize;
        let mut col_index = vec![0u32; nnz];
        let mut etype = vec![0u8; nnz];
        let mut side = vec![0u8; nnz];

        let mut cur = row_index[..num_observed].to_vec();
        for i in 0..num_observed {
            // Parents (side=1 means arrow pointing at us)
            for &p in &parents[i] {
                let k = cur[i] as usize;
                col_index[k] = p;
                etype[k] = dir;
                side[k] = 1; // head side (arrow points to this node)
                cur[i] += 1;
            }
            // Spouses (bidirected, side=0 by convention for symmetric edges)
            for &s in &spouses[i] {
                let k = cur[i] as usize;
                col_index[k] = s;
                etype[k] = bid;
                side[k] = 0;
                cur[i] += 1;
            }
            // Children (side=0 means tail, arrow pointing away)
            for &c in &children[i] {
                let k = cur[i] as usize;
                col_index[k] = c;
                etype[k] = dir;
                side[k] = 0; // tail side (arrow points away from this node)
                cur[i] += 1;
            }
        }

        let core = CaugiGraph::from_csr(
            row_index,
            col_index,
            etype,
            side,
            true,
            self.core_ref().registry.clone(),
        )?;
        Admg::new(Arc::new(core))
    }

    /// Convert DAG to CPDAG using Meek's rules.
    pub fn to_cpdag(&self) -> Result<Pdag, String> {
        let n = self.n() as usize;

        let mut pa: Vec<HashSet<u32>> = vec![HashSet::new(); n];
        let mut ch: Vec<HashSet<u32>> = vec![HashSet::new(); n];
        let mut und: Vec<HashSet<u32>> = vec![HashSet::new(); n];

        #[inline]
        fn adjacent(
            a: usize,
            b: usize,
            und: &[HashSet<u32>],
            pa: &[HashSet<u32>],
            ch: &[HashSet<u32>],
        ) -> bool {
            und[a].contains(&(b as u32))
                || und[b].contains(&(a as u32))
                || pa[a].contains(&(b as u32))
                || ch[a].contains(&(b as u32))
                || pa[b].contains(&(a as u32))
                || ch[b].contains(&(a as u32))
        }

        #[inline]
        fn orient(
            a: u32,
            b: u32,
            und: &mut [HashSet<u32>],
            pa: &mut [HashSet<u32>],
            ch: &mut [HashSet<u32>],
        ) {
            let ai = a as usize;
            let bi = b as usize;
            und[ai].remove(&b);
            und[bi].remove(&a);
            ch[ai].insert(b);
            pa[bi].insert(a);
        }

        fn has_dir_path(ch: &[HashSet<u32>], src: u32, tgt: u32) -> bool {
            if src == tgt {
                return true;
            }
            let n = ch.len();
            let mut seen = vec![false; n];
            let mut q = VecDeque::new();
            q.push_back(src);
            while let Some(u) = q.pop_front() {
                if u == tgt {
                    return true;
                }
                if std::mem::replace(&mut seen[u as usize], true) {
                    continue;
                }
                for &v in &ch[u as usize] {
                    if !seen[v as usize] {
                        q.push_back(v);
                    }
                }
            }
            false
        }

        // Skeleton from DAG (undirected)
        for u in 0..self.n() {
            for &v in self.children_of(u) {
                und[u as usize].insert(v);
                und[v as usize].insert(u);
            }
        }

        // Orient v-structures: a->b<-c with a !~ c
        for b in 0..self.n() {
            let parents = self.parents_of(b).to_vec();
            for i in 0..parents.len() {
                for j in (i + 1)..parents.len() {
                    let a = parents[i] as usize;
                    let c = parents[j] as usize;
                    if !adjacent(a, c, &und, &pa, &ch) {
                        orient(parents[i], b, &mut und, &mut pa, &mut ch);
                        orient(parents[j], b, &mut und, &mut pa, &mut ch);
                    }
                }
            }
        }

        // Meek closure (R1–R4)
        loop {
            let mut changed = false;

            // R1: a->b, b--c, a !~ c  ⇒  b->c
            for b in 0..n {
                if pa[b].is_empty() || und[b].is_empty() {
                    continue;
                }
                let pb: Vec<u32> = pa[b].iter().copied().collect();
                let ubs: Vec<u32> = und[b].clone().into_iter().collect();
                'c_loop: for c in ubs {
                    let ci = c as usize;
                    for &a in &pb {
                        if !adjacent(a as usize, ci, &und, &pa, &ch) {
                            orient(b as u32, c, &mut und, &mut pa, &mut ch);
                            changed = true;
                            continue 'c_loop;
                        }
                    }
                }
            }

            // R2: a--b and ∃ w: a->w, w->b  ⇒  a->b
            for a in 0..n {
                let uab: Vec<u32> = und[a].clone().into_iter().collect();
                for b_u in uab {
                    let b = b_u as usize;
                    if ch[a].iter().any(|w| pa[b].contains(w)) {
                        orient(a as u32, b_u, &mut und, &mut pa, &mut ch);
                        changed = true;
                        continue;
                    }
                    if ch[b].iter().any(|w| pa[a].contains(w)) {
                        orient(b_u, a as u32, &mut und, &mut pa, &mut ch);
                        changed = true;
                    }
                }
            }

            // R3: a--b and ∃ c,d: c->b, d->b, c !~ d, a--c, a--d  ⇒  a->b
            for a in 0..n {
                let uab: Vec<u32> = und[a].clone().into_iter().collect();
                for b_u in uab {
                    let b = b_u as usize;
                    let pb: Vec<u32> = pa[b].iter().copied().collect();
                    'pairs: for i in 0..pb.len() {
                        for j in (i + 1)..pb.len() {
                            let c = pb[i] as usize;
                            let d = pb[j] as usize;
                            if !adjacent(c, d, &und, &pa, &ch)
                                && und[a].contains(&pb[i])
                                && und[a].contains(&pb[j])
                            {
                                orient(a as u32, b_u, &mut und, &mut pa, &mut ch);
                                changed = true;
                                break 'pairs;
                            }
                        }
                    }
                }
            }

            // R4: a--b and (a ⇒ b or b ⇒ a)  ⇒  orient along reachability
            for a in 0..n {
                let uab: Vec<u32> = und[a].clone().into_iter().collect();
                for b_u in uab {
                    if has_dir_path(&ch, a as u32, b_u) {
                        orient(a as u32, b_u, &mut und, &mut pa, &mut ch);
                        changed = true;
                    } else if has_dir_path(&ch, b_u, a as u32) {
                        orient(b_u, a as u32, &mut und, &mut pa, &mut ch);
                        changed = true;
                    }
                }
            }

            if !changed {
                break;
            }
        }

        // Build CSR core (parents | undirected | children)
        let specs = &self.core_ref().registry.specs;
        let mut dir_code: Option<u8> = None;
        let mut und_code: Option<u8> = None;
        for (i, s) in specs.iter().enumerate() {
            match s.class {
                EdgeClass::Directed => {
                    if dir_code.is_none() || s.glyph == "-->" {
                        dir_code = Some(i as u8)
                    }
                }
                EdgeClass::Undirected => {
                    if und_code.is_none() || s.glyph == "---" {
                        und_code = Some(i as u8)
                    }
                }
                _ => {}
            }
        }
        let dir = dir_code.ok_or("No Directed edge spec in registry")?;
        let undc = und_code.ok_or("No Undirected edge spec in registry")?;

        let mut row_index = Vec::with_capacity(n + 1);
        row_index.push(0u32);
        for i in 0..n {
            let c = pa[i].len() + und[i].len() + ch[i].len();
            row_index.push(row_index[i] + c as u32);
        }
        let nnz = *row_index.last().unwrap() as usize;
        let mut col_index = vec![0u32; nnz];
        let mut etype = vec![0u8; nnz];
        let mut side = vec![0u8; nnz];

        let mut cur = row_index[..n].to_vec();
        for i in 0..n {
            // parents
            let mut v: Vec<u32> = pa[i].iter().copied().collect();
            v.sort_unstable();
            for p in v {
                let k = cur[i] as usize;
                col_index[k] = p;
                etype[k] = dir;
                side[k] = 1;
                cur[i] += 1;
            }
            // undirected
            let mut v: Vec<u32> = und[i].iter().copied().collect();
            v.sort_unstable();
            for u in v {
                let k = cur[i] as usize;
                col_index[k] = u;
                etype[k] = undc;
                side[k] = 0;
                cur[i] += 1;
            }
            // children
            let mut v: Vec<u32> = ch[i].iter().copied().collect();
            v.sort_unstable();
            for c in v {
                let k = cur[i] as usize;
                col_index[k] = c;
                etype[k] = dir;
                side[k] = 0;
                cur[i] += 1;
            }
        }

        let core = CaugiGraph::from_csr(
            row_index,
            col_index,
            etype,
            side,
            /*simple=*/ true,
            self.core_ref().registry.clone(),
        )?;
        Pdag::new(Arc::new(core))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn dag_to_cpdag_basic() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let cpdag = dag.to_cpdag().unwrap();

        assert_eq!(cpdag.parents_of(1), vec![0, 2]);
        assert_eq!(cpdag.children_of(0), vec![1]);
        assert_eq!(cpdag.children_of(2), vec![1]);
    }

    #[test]
    fn dag_skeleton_basic() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        let ug = dag.skeleton().unwrap();
        assert_eq!(ug.neighbors_of(0), &[1, 2]);
        assert_eq!(ug.neighbors_of(1), &[0]);
        assert_eq!(ug.neighbors_of(2), &[0]);
    }

    #[test]
    fn dag_moralize_married_parents() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();
        let ug = dag.moralize().unwrap();
        assert_eq!(ug.neighbors_of(0), &[1, 2]);
        assert_eq!(ug.neighbors_of(1), &[0, 2]);
        assert_eq!(ug.neighbors_of(2), &[0, 1]);
    }

    // ── Latent projection tests ──────────────────────────────────────────────

    #[test]
    fn latent_project_basic_confounding() {
        // DAG: U -> X, U -> Y, X -> Y
        // Project out U
        // Result: X -> Y only (no bidirected edge because there's already a directed edge)
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        // 0:U, 1:X, 2:Y
        b.add_edge(0, 1, d).unwrap(); // U -> X
        b.add_edge(0, 2, d).unwrap(); // U -> Y
        b.add_edge(1, 2, d).unwrap(); // X -> Y
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let admg = dag.latent_project(&[0]).unwrap();

        // Result should have 2 nodes
        assert_eq!(admg.n(), 2);

        // X -> Y preserved (new indices: X=0, Y=1)
        assert_eq!(admg.parents_of(1), &[0]); // Y has parent X
        assert_eq!(admg.children_of(0), &[1]); // X has child Y

        // NO bidirected edge: X and Y share latent U, but there's already X -> Y
        assert!(admg.spouses_of(0).is_empty());
        assert!(admg.spouses_of(1).is_empty());
    }

    #[test]
    fn latent_project_no_shared_latent() {
        // DAG: X -> Y (no latent confounding)
        // Project out nothing, result: X -> Y
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, d).unwrap(); // X -> Y
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let admg = dag.latent_project(&[]).unwrap();

        assert_eq!(admg.n(), 2);
        assert_eq!(admg.parents_of(1), &[0]);
        assert!(admg.spouses_of(0).is_empty());
        assert!(admg.spouses_of(1).is_empty());
    }

    #[test]
    fn latent_project_multiple_latents() {
        // DAG: L1 -> X, L1 -> Y, L2 -> Y, L2 -> Z, X -> Y, Y -> Z
        // Project out L1, L2
        // Result: X -> Y -> Z, plus only X <-> Z
        // (X <-> Y and Y <-> Z are NOT added because of existing directed edges)
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        // 0:L1, 1:L2, 2:X, 3:Y, 4:Z
        b.add_edge(0, 2, d).unwrap(); // L1 -> X
        b.add_edge(0, 3, d).unwrap(); // L1 -> Y
        b.add_edge(1, 3, d).unwrap(); // L2 -> Y
        b.add_edge(1, 4, d).unwrap(); // L2 -> Z
        b.add_edge(2, 3, d).unwrap(); // X -> Y
        b.add_edge(3, 4, d).unwrap(); // Y -> Z
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let admg = dag.latent_project(&[0, 1]).unwrap();

        // Result has 3 nodes: X=0, Y=1, Z=2
        assert_eq!(admg.n(), 3);

        // Directed edges preserved
        assert_eq!(admg.children_of(0), &[1]); // X -> Y
        assert_eq!(admg.children_of(1), &[2]); // Y -> Z
        assert_eq!(admg.parents_of(1), &[0]); // Y has parent X
        assert_eq!(admg.parents_of(2), &[1]); // Z has parent Y

        // Bidirected edges from latent confounding:
        // - X <-> Y: skipped (X -> Y exists)
        // - Y <-> Z: skipped (Y -> Z exists)
        // - X <-> Z: added (no direct edge, share L1 as ancestor)
        assert_eq!(admg.spouses_of(0), &[2]); // X <-> Z only
        assert!(admg.spouses_of(1).is_empty()); // Y has no spouses
        assert_eq!(admg.spouses_of(2), &[0]); // Z <-> X only
    }

    #[test]
    fn latent_project_latent_chain() {
        // DAG: L1 -> L2 -> X, L2 -> Y
        // Project out L1, L2
        // X and Y share latent ancestor L2, so X <-> Y
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // 0:L1, 1:L2, 2:X, 3:Y
        b.add_edge(0, 1, d).unwrap(); // L1 -> L2
        b.add_edge(1, 2, d).unwrap(); // L2 -> X
        b.add_edge(1, 3, d).unwrap(); // L2 -> Y
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let admg = dag.latent_project(&[0, 1]).unwrap();

        // Result has 2 nodes: X=0, Y=1
        assert_eq!(admg.n(), 2);

        // No directed edges between X and Y
        assert!(admg.children_of(0).is_empty());
        assert!(admg.children_of(1).is_empty());
        assert!(admg.parents_of(0).is_empty());
        assert!(admg.parents_of(1).is_empty());

        // X <-> Y from shared ancestor L2
        assert_eq!(admg.spouses_of(0), &[1]);
        assert_eq!(admg.spouses_of(1), &[0]);
    }

    #[test]
    fn latent_project_all_latent() {
        // DAG: L1 -> L2
        // Project out both, result: empty ADMG
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let admg = dag.latent_project(&[0, 1]).unwrap();
        assert_eq!(admg.n(), 0);
    }

    #[test]
    fn latent_project_invalid_index() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let result = dag.latent_project(&[5]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("out of bounds"));
    }

    #[test]
    fn latent_project_no_edge_confounding() {
        // DAG: U -> X, V -> Y (no shared ancestor)
        // Project out U, V
        // Result: X, Y (isolated nodes, no edges)
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        // 0:U, 1:V, 2:X, 3:Y
        b.add_edge(0, 2, d).unwrap(); // U -> X
        b.add_edge(1, 3, d).unwrap(); // V -> Y
        let dag = Dag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let admg = dag.latent_project(&[0, 1]).unwrap();

        // Result has 2 nodes
        assert_eq!(admg.n(), 2);

        // No edges at all
        assert!(admg.children_of(0).is_empty());
        assert!(admg.children_of(1).is_empty());
        assert!(admg.spouses_of(0).is_empty());
        assert!(admg.spouses_of(1).is_empty());
    }
}
