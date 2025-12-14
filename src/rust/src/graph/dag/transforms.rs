// SPDX-License-Identifier: MIT
//! Graph transformations for DAGs: skeleton, moralize, to_cpdag.

use super::Dag;
use crate::edges::EdgeClass;
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
}
