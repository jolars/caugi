// SPDX-License-Identifier: MIT
//! Pdag wrapper with O(1) slice queries via packed neighborhoods.

use super::CaugiGraph;
use crate::edges::EdgeClass;
use crate::graph::alg::directed_part_is_acyclic;
use std::sync::Arc;
use crate::graph::ug::Ug;

#[derive(Debug, Clone)]
pub struct Pdag {
    core: Arc<CaugiGraph>,
    /// len = n+1
    node_edge_ranges: Arc<[usize]>,
    /// len = n; (parents, undirected, children)
    node_deg: Arc<[(u32, u32, u32)]>,
    /// packed as [parents | undirected | children]
    neighborhoods: Arc<[u32]>,
}

impl Pdag {
    pub fn new(core: Arc<CaugiGraph>) -> Result<Self, String> {
        let n = core.n() as usize;
        if !directed_part_is_acyclic(&core) {
            return Err("PDAG contains a directed cycle".into());
        }
        let mut deg: Vec<(u32, u32, u32)> = vec![(0, 0, 0); n];
        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Directed => {
                        if core.side[k] == 1 {
                            deg[i].0 += 1
                        } else {
                            deg[i].2 += 1
                        }
                    }
                    EdgeClass::Undirected => deg[i].1 += 1,
                    // Throw error on partial/bidirected edges
                    _ => {
                        return Err("Pdag cannot contain partial/bidirected edges".into());
                    }
                }
            }
        }
        let mut node_edge_ranges = Vec::with_capacity(n + 1);
        node_edge_ranges.push(0usize);
        for i in 0..n {
            let (pa, u, ch) = deg[i];
            let last = *node_edge_ranges.last().unwrap();
            node_edge_ranges.push(last + (pa + u + ch) as usize);
        }
        let total = *node_edge_ranges.last().unwrap();
        let mut neigh = vec![0u32; total];

        // bucket bases
        let mut parent_base: Vec<usize> = vec![0; n];
        let mut und_base: Vec<usize> = vec![0; n];
        let mut child_base: Vec<usize> = vec![0; n];
        for i in 0..n {
            let start = node_edge_ranges[i];
            let (pa, u, _) = deg[i];
            parent_base[i] = start;
            und_base[i] = start + pa as usize;
            child_base[i] = und_base[i] + u as usize;
        }
        let mut pcur = parent_base.clone();
        let mut ucur = und_base.clone();
        let mut ccur = child_base.clone();

        for i in 0..n {
            let r = core.row_range(i as u32);
            for k in r.clone() {
                let spec = &core.registry.specs[core.etype[k] as usize];
                match spec.class {
                    EdgeClass::Directed => {
                        if core.side[k] == 1 {
                            let p = pcur[i];
                            neigh[p] = core.col_index[k];
                            pcur[i] += 1;
                        } else {
                            let p = ccur[i];
                            neigh[p] = core.col_index[k];
                            ccur[i] += 1;
                        }
                    }
                    EdgeClass::Undirected => {
                        let p = ucur[i];
                        neigh[p] = core.col_index[k];
                        ucur[i] += 1;
                    }
                    _ => {
                        // Is only here to satisfy exhaustiveness. It's unreachable
                        unreachable!("Should have errored on partial/bidirected edges earlier");
                    }
                }
            }
            // determinism
            let s = node_edge_ranges[i];
            let pm = und_base[i];
            let um = child_base[i];
            let e = node_edge_ranges[i + 1];
            neigh[s..pm].sort_unstable();
            neigh[pm..um].sort_unstable();
            neigh[um..e].sort_unstable();
        }

        Ok(Self {
            core,
            node_edge_ranges: node_edge_ranges.into(),
            node_deg: deg.into(),
            neighborhoods: neigh.into(),
        })
    }

    #[inline]
    pub fn n(&self) -> u32 {
        self.core.n()
    }
    #[inline]
    fn bounds(&self, i: u32) -> (usize, usize, usize, usize) {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        let (pa, u, ch) = self.node_deg[i];
        let pm = s + pa as usize;
        let um = pm + u as usize;
        let cs = e - ch as usize;
        (s, pm, um, cs)
    }

    #[inline]
    pub fn parents_of(&self, i: u32) -> &[u32] {
        let (s, pm, _, _) = self.bounds(i);
        &self.neighborhoods[s..pm]
    }
    #[inline]
    pub fn children_of(&self, i: u32) -> &[u32] {
        let (_, _, _, cs) = self.bounds(i);
        let e = self.node_edge_ranges[i as usize + 1];
        &self.neighborhoods[cs..e]
    }
    #[inline]
    pub fn undirected_of(&self, i: u32) -> &[u32] {
        let (_, pm, um, _) = self.bounds(i);
        &self.neighborhoods[pm..um]
    }

    #[inline]
    pub fn neighbors_of(&self, i: u32) -> &[u32] {
        let i = i as usize;
        let s = self.node_edge_ranges[i];
        let e = self.node_edge_ranges[i + 1];
        &self.neighborhoods[s..e]
    }

    #[inline]
    pub fn ancestors_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut out = Vec::new();
        let mut stack: Vec<u32> = self.parents_of(i).to_vec();
        while let Some(u) = stack.pop() {
            let ui = u as usize;
            if seen[ui] {
                continue;
            }
            seen[ui] = true;
            out.push(u);
            stack.extend_from_slice(self.parents_of(u));
        }
        out.sort_unstable();
        out
    }
    #[inline]
    pub fn descendants_of(&self, i: u32) -> Vec<u32> {
        let n = self.n() as usize;
        let mut seen = vec![false; n];
        let mut out = Vec::new();
        let mut stack: Vec<u32> = self.children_of(i).to_vec();
        while let Some(u) = stack.pop() {
            let ui = u as usize;
            if seen[ui] {
                continue;
            }
            seen[ui] = true;
            out.push(u);
            stack.extend_from_slice(self.children_of(u));
        }
        out.sort_unstable();
        out
    }
    #[inline]
    pub fn markov_blanket_of(&self, i: u32) -> Vec<u32> {
        let mut mb: Vec<u32> = Vec::new();
        // DAG part
        mb.extend_from_slice(self.parents_of(i));
        mb.extend_from_slice(self.children_of(i));
        for &c in self.children_of(i) {
            for &p in self.parents_of(c) {
                if p != i {
                    mb.push(p);
                }
            }
        }
        // undirected neighbors belong to the blanket in (C)PDAGs
        mb.extend_from_slice(self.undirected_of(i));
        mb.sort_unstable();
        mb.dedup();
        mb
    }
    #[inline]
    pub fn exogenous_nodes(&self, undirected_as_parents: bool) -> Vec<u32> {
        (0..self.n())
            .filter(|&i| {
                let no_pa = self.parents_of(i).is_empty();
                if undirected_as_parents {
                    no_pa && self.undirected_of(i).is_empty()
                } else {
                    no_pa
                }
            })
            .collect()
    }

    pub fn core_ref(&self) -> &CaugiGraph {
        &self.core
    }
}

impl Pdag {
    /// Full CPDAG check. Returns true iff `self` is a CPDAG.
    pub fn is_cpdag(&self) -> bool {
        let n = self.n() as usize;
        if n <= 1 {
            return true;
        }

        // chain components
        let (comp, c) = self.chain_components();

        // no arrows within any undirected component
        if self.has_intra_component_arrows(&comp) {
            return false;
        }

        // component DAG must be acyclic
        if !self.component_dag_is_acyclic(&comp, c) {
            return false;
        }

        // each undirected component is chordal
        if !self.components_are_chordal(&comp, c) {
            return false;
        }

        // Meeks rules would not orient any more edges
        if !self.meeks_rules_blocked() {
            return false;
        }

        // every arrow is strongly protected
        if !self.all_arrows_strongly_protected(&comp) {
            return false;
        }

        true
    }
}

impl Pdag {
    // helpers for is_cpdag
    #[inline]
    fn adjacent(&self, a: u32, b: u32) -> bool {
        // neighbors are sorted; [parents | undirected | children]
        self.neighbors_of(a).binary_search(&b).is_ok()
    }

    #[inline]
    fn intersects_sorted(a: &[u32], b: &[u32]) -> bool {
        use std::cmp::Ordering::*;
        let (mut i, mut j) = (0usize, 0usize);
        while i < a.len() && j < b.len() {
            match a[i].cmp(&b[j]) {
                Less => i += 1,
                Greater => j += 1,
                Equal => return true,
            }
        }
        false
    }

    #[inline]
    fn has_dir_path(&self, src: u32, tgt: u32) -> bool {
        let n = self.n() as usize;
        if src == tgt {
            return true;
        }
        let mut seen = vec![false; n];
        use std::collections::VecDeque;
        let mut q = VecDeque::new();
        q.push_back(src);
        while let Some(u) = q.pop_front() {
            if u == tgt {
                return true;
            }
            if std::mem::replace(&mut seen[u as usize], true) {
                continue;
            }
            for &w in self.children_of(u) {
                if !seen[w as usize] {
                    q.push_back(w);
                }
            }
        }
        false
    }

    /// DFS over undirected edges to get chain components.
    fn chain_components(&self) -> (Vec<usize>, usize) {
        let n = self.n() as usize;
        let mut comp = vec![usize::MAX; n];
        let mut st = Vec::new();
        let mut cid = 0usize;

        for s in 0..n {
            if comp[s] != usize::MAX {
                continue;
            }
            comp[s] = cid;
            st.clear();
            st.push(s as u32);
            while let Some(u) = st.pop() {
                for &w in self.undirected_of(u) {
                    let wi = w as usize;
                    if comp[wi] == usize::MAX {
                        comp[wi] = cid;
                        st.push(w);
                    }
                }
            }
            cid += 1;
        }
        (comp, cid)
    }

    /// True if any directed edge stays inside an undirected component.
    fn has_intra_component_arrows(&self, comp: &[usize]) -> bool {
        for u in 0..self.n() {
            let cu = comp[u as usize];
            for &p in self.parents_of(u) {
                if comp[p as usize] == cu {
                    return true;
                }
            }
            for &v in self.children_of(u) {
                if comp[v as usize] == cu {
                    return true;
                }
            }
        }
        false
    }

    /// Kahn on the component DAG.
    fn component_dag_is_acyclic(&self, comp: &[usize], c: usize) -> bool {
        use std::collections::{HashSet, VecDeque};
        let mut succ: Vec<Vec<usize>> = vec![Vec::new(); c];
        let mut indeg = vec![0usize; c];
        let mut seen = HashSet::<(usize, usize)>::new();

        for u in 0..self.n() {
            let cu = comp[u as usize];
            for &v in self.children_of(u) {
                let cv = comp[v as usize];
                if cu != cv && seen.insert((cu, cv)) {
                    succ[cu].push(cv);
                    indeg[cv] += 1;
                }
            }
        }

        let mut q: VecDeque<usize> = (0..c).filter(|&k| indeg[k] == 0).collect();
        let mut seen_cnt = 0usize;
        while let Some(x) = q.pop_front() {
            seen_cnt += 1;
            for &y in &succ[x] {
                indeg[y] -= 1;
                if indeg[y] == 0 {
                    q.push_back(y);
                }
            }
        }
        seen_cnt == c
    }

    /// Check chordality of all undirected components via MCS + clique test.
    fn components_are_chordal(&self, comp: &[usize], c: usize) -> bool {
        let n = self.n() as usize;
        let mut nodes_in: Vec<Vec<usize>> = vec![Vec::new(); c];
        for v in 0..n {
            nodes_in[comp[v]].push(v);
        }

        let mut label = vec![0usize; n];
        let mut numbered = vec![false; n];
        let mut in_c = vec![false; n];
        let mut pos = vec![0usize; n];

        for nodes in nodes_in.into_iter() {
            if nodes.len() <= 2 {
                continue;
            }

            for &v in &nodes {
                in_c[v] = true;
                numbered[v] = false;
                label[v] = 0;
            }

            // MCS order
            let mut order = Vec::with_capacity(nodes.len());
            for _ in 0..nodes.len() {
                let mut pick = usize::MAX;
                let mut best = 0usize;
                for &v in &nodes {
                    if !numbered[v] && (pick == usize::MAX || label[v] > best) {
                        pick = v;
                        best = label[v];
                    }
                }
                if pick == usize::MAX {
                    for &v in &nodes {
                        in_c[v] = false;
                    }
                    return false;
                }
                order.push(pick);
                numbered[pick] = true;
                for &w in self.undirected_of(pick as u32) {
                    let wi = w as usize;
                    if in_c[wi] && !numbered[wi] {
                        label[wi] += 1;
                    }
                }
            }

            for (i, &v) in order.iter().rev().enumerate() {
                pos[v] = i;
            }
            // clique test
            for (i, &v) in order.iter().rev().enumerate() {
                let mut higher: Vec<usize> = self
                    .undirected_of(v as u32)
                    .iter()
                    .map(|&w| w as usize)
                    .filter(|&w| in_c[w] && pos[w] > i)
                    .collect();

                if higher.len() <= 1 {
                    continue;
                }

                higher.sort_unstable_by_key(|&w| pos[w]);
                let pvt = *higher.last().unwrap();
                for &w in &higher[..higher.len() - 1] {
                    if !self
                        .undirected_of(w as u32)
                        .binary_search(&(pvt as u32))
                        .is_ok()
                    {
                        for &x in &nodes {
                            in_c[x] = false;
                        }
                        return false;
                    }
                }
            }

            for &v in &nodes {
                in_c[v] = false;
            }
        }

        true
    }

    /// No Meeks rule applies (R1..R4).
    fn meeks_rules_blocked(&self) -> bool {
        // R1: u->v, v--w, u !~ w
        for v in 0..self.n() {
            let pa = self.parents_of(v);
            if pa.is_empty() {
                continue;
            }
            for &w in self.undirected_of(v) {
                if pa.iter().any(|&u| !self.adjacent(u, w)) {
                    return false;
                }
            }
        }

        // R2: u--v and ∃ w with u->w, w->v
        for v in 0..self.n() {
            let pa_v = self.parents_of(v);
            for &u in self.undirected_of(v) {
                if Self::intersects_sorted(self.children_of(u), pa_v) {
                    return false;
                }
            }
        }

        // R3: u--v and ∃ w,x: w->v, x->v, w !~ x, u--w, u--x
        for v in 0..self.n() {
            let pv = self.parents_of(v);
            if pv.len() < 2 {
                continue;
            }
            for &u in self.undirected_of(v) {
                let und_u = self.undirected_of(u);
                for i in 0..pv.len() {
                    for j in (i + 1)..pv.len() {
                        let (w, x) = (pv[i], pv[j]);
                        if !self.adjacent(w, x)
                            && und_u.binary_search(&w).is_ok()
                            && und_u.binary_search(&x).is_ok()
                        {
                            return false;
                        }
                    }
                }
            }
        }

        // R4: u--v and (u ⇒ v or v ⇒ u)
        for v in 0..self.n() {
            for &u in self.undirected_of(v) {
                if self.has_dir_path(u, v) || self.has_dir_path(v, u) {
                    return false;
                }
            }
        }

        true
    }

    /// Every arrow is strongly protected (SP0..SP4).
    fn all_arrows_strongly_protected(&self, comp: &[usize]) -> bool {
        for a in 0..self.n() {
            for &b in self.children_of(a) {
                // SP0: across components edges have fixed direction
                if comp[a as usize] != comp[b as usize] {
                    continue;
                }
                // SP1: ∃ c: c->a and c !~ b
                if self.parents_of(a).iter().any(|&c| !self.adjacent(c, b)) {
                    continue;
                }
                // SP2: ∃ c: a->c and c->b
                if Self::intersects_sorted(self.children_of(a), self.parents_of(b)) {
                    continue;
                }
                // SP3: ∃ c,d: c->b, d->b, c !~ d, a--c, a--d
                let pb = self.parents_of(b);
                let und_a = self.undirected_of(a);
                let mut sp3 = false;
                for i in 0..pb.len() {
                    for j in (i + 1)..pb.len() {
                        let (c1, c2) = (pb[i], pb[j]);
                        if !self.adjacent(c1, c2)
                            && und_a.binary_search(&c1).is_ok()
                            && und_a.binary_search(&c2).is_ok()
                        {
                            sp3 = true;
                            break;
                        }
                    }
                    if sp3 {
                        break;
                    }
                }
                if sp3 {
                    continue;
                }
                // SP4: ∃ c: a--c and c ⇒ b
                for &c in und_a {
                    if self.has_dir_path(c, b) {
                        // protected
                        continue;
                    }
                }
                // not strongly protected
                return false;
            }
        }
        true
    }
}

impl Pdag {
    fn undirected_code(&self) -> Result<u8, String> {
        let mut code: Option<u8> = None;
        for (i, s) in self.core_ref().registry.specs.iter().enumerate() {
            if let EdgeClass::Undirected = s.class {
                if code.is_none() || s.glyph == "---" {
                    code = Some(i as u8);
                }
            }
        }
        code.ok_or("No Undirected edge spec in registry".into())
    }

    fn build_ug_core_from_adj(&self, adj: &[Vec<u32>]) -> Result<CaugiGraph, String> {
        let n = self.n() as usize;
        let und = self.undirected_code()?;

        let mut row_index = Vec::with_capacity(n + 1);
        row_index.push(0u32);
        for i in 0..n {
            row_index.push(row_index[i] + adj[i].len() as u32);
        }
        let nnz = *row_index.last().unwrap() as usize;

        let mut col_index = vec![0u32; nnz];
        let etype = vec![und; nnz];
        let side = vec![0u8; nnz];

        let mut cur = row_index[..n].to_vec();
        for i in 0..n {
            for &v in &adj[i] {
                let k = cur[i] as usize;
                col_index[k] = v;
                cur[i] += 1;
            }
        }

        CaugiGraph::from_csr(
            row_index,
            col_index,
            etype,
            side,
            /*simple=*/ true,
            self.core_ref().registry.clone(),
        )
    }

    /// UG skeleton of a PDAG: ignore orientation and keep every adjacency once per side.
    pub fn skeleton(&self) -> Result<Ug, String> {
        let n = self.n() as usize;
        let mut adj = vec![Vec::<u32>::new(); n];
        for i in 0..n {
            let mut v = self.neighbors_of(i as u32).to_vec(); // [parents | undirected | children]
            v.sort_unstable();
            v.dedup();
            adj[i] = v;
        }
        let core = self.build_ug_core_from_adj(&adj)?;
        Ug::new(Arc::new(core))
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn pdag_relations() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let g = Pdag::new(core).expect("Pdag construction failed");
        assert_eq!(g.parents_of(1), vec![0]);
        assert_eq!(g.children_of(0), vec![1]);
        let mut u = g.undirected_of(1).to_vec();
        u.sort_unstable();
        assert_eq!(u, vec![2]);
        assert_eq!(g.n(), 3);
        assert_eq!(g.neighbors_of(0), vec![1]);
        assert_eq!(g.neighbors_of(1), vec![0, 2]);
        assert_eq!(g.neighbors_of(2), vec![1]);

        // get core
        let core = g.core_ref();
        assert_eq!(core.n(), 3);
    }

    #[test]
    fn pdag_cycle_error() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cdir).unwrap();
        b.add_edge(2, 0, cdir).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let r = Pdag::new(core);
        assert!(r.is_err());
    }

    #[test]
    fn pdag_an_de_directed_only() {
        // 0 -> 1 -- 2
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cdir = reg.code_of("-->").unwrap();
        let cund = reg.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, cdir).unwrap();
        b.add_edge(1, 2, cund).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.ancestors_of(1), vec![0]);
        assert_eq!(g.descendants_of(0), vec![1]);
        assert!(g.ancestors_of(2).is_empty());
        assert!(g.descendants_of(2).is_empty());
    }

    #[test]
    fn pdag_mb() {
        // 0 -> 1 -- 2, and 1 <- 3
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let u = r.code_of("---").unwrap();
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(3, 1, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.markov_blanket_of(1), vec![0, 2, 3]); // parents {0,3}, undirected {2}
        assert_eq!(g.markov_blanket_of(0), vec![1, 3]); // child {1}, co-parent via 1 {3}
    }
    #[test]
    fn pdag_partial_edge_error() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let cpar = reg.code_of("o->").unwrap();
        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, cpar).unwrap();
        let core = std::sync::Arc::new(b.finalize().unwrap());
        let r = Pdag::new(core);
        assert!(r.is_err());
    }

    #[test]
    fn pdag_exogenous() {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        let d = r.code_of("-->").unwrap();
        let u = r.code_of("---").unwrap();
        // 0->1, 1--2; node 3 isolated
        let mut b = GraphBuilder::new_with_registry(4, true, &r);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert_eq!(g.exogenous_nodes(false), vec![0, 2, 3]); // undirected ignored
        assert_eq!(g.exogenous_nodes(true), vec![0, 3]); // undirected count as parents
    }

    // return edge codes as u8 to match GraphBuilder::add_edge signature
    fn setup() -> (EdgeRegistry, u8, u8) {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d: u8 = reg.code_of("-->").unwrap();
        let u: u8 = reg.code_of("---").unwrap();
        (reg, d, u)
    }

    #[test]
    fn cpdag_trivial_undirected_is_cpdag() {
        // 0 -- 1
        let (reg, _d, u) = setup();

        let mut b = GraphBuilder::new_with_registry(2, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        let g = Pdag::new(std::sync::Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_triangle_standard_is_cpdag() {
        // 0 -> 1, 0 -> 2, 1 -- 2  (classic CPDAG)
        let (reg, d, u) = setup();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        let g = Pdag::new(std::sync::Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_non_chordal_component_rejected() {
        // 0 -- 1 -- 2 -- 3 -- 0 (4-cycle, no chord) => not CPDAG
        let (reg, _d, u) = setup();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(2, 3, u).unwrap();
        b.add_edge(3, 0, u).unwrap();
        let g = Pdag::new(std::sync::Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_meek_closure_would_change_rejected() {
        // 0 -> 1, 1 -- 2, and 0 not adj to 2. Meek R1 would orient 1->2 => not CPDAG.
        let (reg, d, u) = setup();

        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        let g = Pdag::new(std::sync::Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_directed_inside_component_rejected() {
        // chain component {1,2,3}: 1--2--3, plus 1->3 inside the same component ⇒ not CPDAG
        let (reg, d, u) = setup();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(2, 3, u).unwrap();
        b.add_edge(1, 3, d).unwrap(); // directed inside the undirected component
        let g = Pdag::new(std::sync::Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_component_dag_cycle_rejected() {
        // Components {0,1} and {2,3}; edges 0->2 and 2->1 give a cycle C0->C1->C0 => not CPDAG.
        let (reg, d, u) = setup();

        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap(); // comp A
        b.add_edge(2, 3, u).unwrap(); // comp B
        b.add_edge(0, 2, d).unwrap(); // A -> B
        b.add_edge(2, 1, d).unwrap(); // B -> A
        let g = Pdag::new(std::sync::Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_singleton_ok() {
        let (reg, _d, _u) = setup();
        let b = GraphBuilder::new_with_registry(1, true, &reg);
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_isolated_nodes_ok() {
        let (reg, _d, _u) = setup();
        let b = GraphBuilder::new_with_registry(5, true, &reg);
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_pure_dag_chain_ok() {
        // 0 -> 1 -> 2 -> 3
        let (reg, d, _u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(2, 3, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_pure_dag_v_structure_ok() {
        // 0 -> 1 <- 2
        let (reg, d, _u) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_undirected_tree_ok() {
        // 0 -- 1 -- 2 -- 3, and 1 -- 4
        let (reg, _d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(2, 3, u).unwrap();
        b.add_edge(1, 4, u).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_undirected_clique4_ok() {
        // K4 is chordal
        let (reg, _d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        for i in 0..4u32 {
            for j in (i + 1)..4u32 {
                b.add_edge(i, j, u).unwrap();
            }
        }
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_non_chordal_5_cycle_rejected() {
        // 0--1--2--3--4--0 without chords
        let (reg, _d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(2, 3, u).unwrap();
        b.add_edge(3, 4, u).unwrap();
        b.add_edge(4, 0, u).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_no_arrows_inside_component_rejected_minimal() {
        // chain component {0,1,2}: 0--1--2, plus 0->2 inside the same component ⇒ not CPDAG
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(0, 2, d).unwrap(); // directs inside component
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_component_dag_two_components_ok() {
        // A={0,1}, B={2,3}; all edges A -> B, R1 cannot orient 2--3
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap(); // A
        b.add_edge(2, 3, u).unwrap(); // B
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(0, 3, d).unwrap();
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(1, 3, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_meeks_r1_rejected_multiple_parents() {
        // parents {0,2} -> 1, 1 -- 3; 0~3, 2 !~ 3
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(1, 3, u).unwrap();
        b.add_edge(0, 3, u).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_meeks_r2_rejected() {
        // 0--1, 0->2, 2->1
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(2, 1, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_meeks_r3_rejected() {
        // u=0, v=1, w=2, x=3
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(2, 1, d).unwrap();
        b.add_edge(3, 1, d).unwrap();
        b.add_edge(0, 2, u).unwrap();
        b.add_edge(0, 3, u).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_meeks_r4_rejected() {
        // 0--1, 0->2->3->1
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(2, 3, d).unwrap();
        b.add_edge(3, 1, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }

    #[test]
    fn cpdag_mixed_components_chordal_ok() {
        // comp A: 0--1--2 with chord 0--2; comp B: 3; A -> 3
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(4, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(0, 2, u).unwrap();
        b.add_edge(0, 3, d).unwrap();
        b.add_edge(1, 3, d).unwrap();
        b.add_edge(2, 3, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_triangle_directed_fan_ok() {
        // 0 -> 1, 0 -> 2, 1 -- 2
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(0, 2, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_component_dag_multilevel_ok() {
        // A={0,1}, B={2}, C={3,4}; A->B->C
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(5, true, &reg);
        b.add_edge(0, 1, u).unwrap(); // A
        b.add_edge(3, 4, u).unwrap(); // C
        b.add_edge(0, 2, d).unwrap(); // A->B
        b.add_edge(1, 2, d).unwrap();
        b.add_edge(2, 3, d).unwrap(); // B->C
        b.add_edge(2, 4, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn cpdag_component_dag_cycle_detected_even_with_many_edges() {
        // A={0,1}, B={2,3}, C={4,5}; A->B, B->C, C->A but no node-level cycle
        let (reg, d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(6, true, &reg);
        b.add_edge(0, 1, u).unwrap(); // A
        b.add_edge(2, 3, u).unwrap(); // B
        b.add_edge(4, 5, u).unwrap(); // C
        // A -> B
        b.add_edge(0, 2, d).unwrap();
        // B -> C
        b.add_edge(3, 4, d).unwrap();
        // C -> A
        b.add_edge(5, 1, d).unwrap();
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(!g.is_cpdag());
    }
    #[test]
    fn cpdag_chordal_undirected_with_chord_ok() {
        // 0--1--2 with chord 0--2 is a clique; CPDAG OK.
        let (reg, _d, u) = setup();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, u).unwrap();
        b.add_edge(1, 2, u).unwrap();
        b.add_edge(0, 2, u).unwrap(); // chord (clique)
        let g = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();
        assert!(g.is_cpdag());
    }

    #[test]
    fn pdag_skeleton_undirects_all_adjacencies() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let d = reg.code_of("-->").unwrap();
        let u = reg.code_of("---").unwrap();

        // 0 -> 1, 1 -- 2
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, d).unwrap();
        b.add_edge(1, 2, u).unwrap();
        let p = Pdag::new(Arc::new(b.finalize().unwrap())).unwrap();

        let ug = p.skeleton().unwrap();
        assert_eq!(ug.neighbors_of(0), &[1]);
        assert_eq!(ug.neighbors_of(1), &[0, 2]);
        assert_eq!(ug.neighbors_of(2), &[1]);
    }

}
