// SPDX-License-Identifier: MIT
//! Graph transformations for PDAGs.

use super::Pdag;
use crate::edges::EdgeClass;
use crate::graph::alg::csr;
use crate::graph::ug::Ug;
use std::collections::{BTreeSet, VecDeque};
use std::sync::Arc;

impl Pdag {
    /// UG skeleton of a PDAG: ignore orientation and keep every adjacency once per side.
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

    /// Orient all compelled edges implied by Meek rules (R1..R4).
    ///
    /// Returns a new PDAG that is closed under Meek orientation rules.
    pub fn meek_closure(&self) -> Result<Pdag, String> {
        let n = self.n() as usize;
        let mut pa: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); n];
        let mut ch: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); n];
        let mut und: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); n];

        for i in 0..n {
            let u = i as u32;
            pa[i].extend(self.parents_of(u).iter().copied());
            ch[i].extend(self.children_of(u).iter().copied());
            und[i].extend(self.undirected_of(u).iter().copied());
        }

        #[inline]
        fn adjacent(
            a: usize,
            b: usize,
            und: &[BTreeSet<u32>],
            pa: &[BTreeSet<u32>],
            ch: &[BTreeSet<u32>],
        ) -> bool {
            let bu = b as u32;
            let au = a as u32;
            und[a].contains(&bu)
                || und[b].contains(&au)
                || pa[a].contains(&bu)
                || ch[a].contains(&bu)
                || pa[b].contains(&au)
                || ch[b].contains(&au)
        }

        #[inline]
        fn orient(
            a: u32,
            b: u32,
            und: &mut [BTreeSet<u32>],
            pa: &mut [BTreeSet<u32>],
            ch: &mut [BTreeSet<u32>],
        ) {
            let ai = a as usize;
            let bi = b as usize;
            und[ai].remove(&b);
            und[bi].remove(&a);
            ch[ai].insert(b);
            pa[bi].insert(a);
        }

        fn has_dir_path(ch: &[BTreeSet<u32>], src: u32, tgt: u32) -> bool {
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

        loop {
            let mut changed = false;

            // R1: a->b, b--c, a !~ c  =>  b->c
            for b in 0..n {
                if pa[b].is_empty() || und[b].is_empty() {
                    continue;
                }
                let pb: Vec<u32> = pa[b].iter().copied().collect();
                let ub: Vec<u32> = und[b].iter().copied().collect();
                'c_loop: for c in ub {
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

            // R2: a--b and exists w: a->w->b  =>  a->b
            for a in 0..n {
                let ab: Vec<u32> = und[a].iter().copied().collect();
                for b_u in ab {
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

            // R3: a--b and exists c,d: c->b, d->b, c !~ d, a--c, a--d  =>  a->b
            for a in 0..n {
                let ab: Vec<u32> = und[a].iter().copied().collect();
                for b_u in ab {
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

            // R4: a--b and (a => b or b => a)  =>  orient along reachability
            for a in 0..n {
                let ab: Vec<u32> = und[a].iter().copied().collect();
                for b_u in ab {
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

        // Build CSR core from [parents | undirected | children].
        let specs = &self.core_ref().registry.specs;
        let mut dir_code: Option<u8> = None;
        let mut und_code: Option<u8> = None;
        for (i, s) in specs.iter().enumerate() {
            match s.class {
                EdgeClass::Directed => {
                    if dir_code.is_none() || s.glyph == "-->" {
                        dir_code = Some(i as u8);
                    }
                }
                EdgeClass::Undirected => {
                    if und_code.is_none() || s.glyph == "---" {
                        und_code = Some(i as u8);
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
            for &p in &pa[i] {
                let k = cur[i] as usize;
                col_index[k] = p;
                etype[k] = dir;
                side[k] = 1;
                cur[i] += 1;
            }
            for &u in &und[i] {
                let k = cur[i] as usize;
                col_index[k] = u;
                etype[k] = undc;
                side[k] = 0;
                cur[i] += 1;
            }
            for &c in &ch[i] {
                let k = cur[i] as usize;
                col_index[k] = c;
                etype[k] = dir;
                side[k] = 0;
                cur[i] += 1;
            }
        }

        let core = crate::graph::CaugiGraph::from_csr(
            row_index,
            col_index,
            etype,
            side,
            true,
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
    fn pdag_skeleton_undirects_all_adjacencies() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();
        let u = reg.code_of("---").unwrap();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        let p = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let ug = p.skeleton().unwrap();
        assert_eq!(ug.neighbors_of(0), &[1]);
        assert_eq!(ug.neighbors_of(1), &[0, 2]);
        assert_eq!(ug.neighbors_of(2), &[1]);
    }

    fn setup() -> (EdgeRegistry, u8, u8) {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();
        let u = reg.code_of("---").unwrap();
        (reg, d, u)
    }

    #[test]
    fn pdag_meek_closure_orients_r1() {
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(1, 3, u).unwrap();
        b.add_edge(0, 3, u).unwrap();
        let p = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let m = p.meek_closure().unwrap();
        assert!(m.children_of(1).contains(&3));
        assert!(!m.undirected_of(1).contains(&3));
        assert!(m.is_meek_closed());
    }

    #[test]
    fn pdag_meek_closure_orients_r2() {
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        let p = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let m = p.meek_closure().unwrap();
        assert!(m.children_of(0).contains(&1));
        assert!(!m.undirected_of(0).contains(&1));
        assert!(m.is_meek_closed());
    }

    #[test]
    fn pdag_meek_closure_orients_r3() {
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(3, 1, d).unwrap();
        b.add_edge(0, 2, u).unwrap();
        b.add_edge(0, 3, u).unwrap();
        let p = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let m = p.meek_closure().unwrap();
        assert!(m.children_of(0).contains(&1));
        assert!(!m.undirected_of(0).contains(&1));
        assert!(m.is_meek_closed());
    }

    #[test]
    fn pdag_meek_closure_orients_r4() {
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(2, 3, d).unwrap();
        b.add_edge(3, 1, d).unwrap();
        let p = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let m = p.meek_closure().unwrap();
        assert!(m.children_of(0).contains(&1));
        assert!(!m.undirected_of(0).contains(&1));
        assert!(m.is_meek_closed());
    }

    #[test]
    fn pdag_meek_closure_is_idempotent() {
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(2, 3, d).unwrap();
        b.add_edge(3, 1, d).unwrap();
        let p = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let m1 = p.meek_closure().unwrap();
        let m2 = m1.meek_closure().unwrap();
        for i in 0..m1.n() {
            assert_eq!(m1.parents_of(i), m2.parents_of(i));
            assert_eq!(m1.undirected_of(i), m2.undirected_of(i));
            assert_eq!(m1.children_of(i), m2.children_of(i));
        }
    }
}
