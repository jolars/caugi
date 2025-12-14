// SPDX-License-Identifier: MIT
//! Graph transformations for PDAGs.

use super::Pdag;
use crate::graph::alg::csr;
use crate::graph::ug::Ug;
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
}
