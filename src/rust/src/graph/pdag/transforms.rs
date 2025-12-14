// SPDX-License-Identifier: MIT
//! Graph transformations for PDAGs.

use super::Pdag;
use crate::edges::EdgeClass;
use crate::graph::ug::Ug;
use crate::graph::CaugiGraph;
use std::sync::Arc;

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
            let mut v = self.neighbors_of(i as u32).to_vec();
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

