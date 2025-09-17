// SPDX-License-Identifier: MIT
//! CSR graph with per-row splits and registry snapshot.

use std::sync::Arc;
use crate::edges::{EdgeSpec};
use crate::edges::QueryFlags;
use std::{hash::{Hash, Hasher}, collections::hash_map::DefaultHasher};
use std::ops::Range;

pub mod builder;

#[derive(Debug, Clone)]
pub struct RegistrySnapshot {
    pub version: u32,
    pub checksum: u64,
    pub specs: Arc<[EdgeSpec]>, // code -> spec
}

impl RegistrySnapshot {
    pub fn from_specs(specs: Arc<[EdgeSpec]>, version: u32) -> Self {
        let mut h = DefaultHasher::new();
        for s in specs.iter() {
            s.glyph.hash(&mut h);
            (s.class as u8).hash(&mut h);
        }
        Self { version, checksum: h.finish(), specs }
    }
}

#[derive(Debug, Clone)]
pub struct CaugiGraph {
    pub row_index: Arc<[u32]>,                 // len n+1
    pub col_index: Arc<[u32]>,                 // len nnz
    pub etype: Arc<[u8]>,                      // len nnz
    pub side: Arc<[u8]>,                       // len nnz; 0=tail, 1=head
    pub split_index: Arc<[(u32,u32,u32)]>,     // len n; (parents, undirs, children)
    pub registry: RegistrySnapshot,
}

/// Named counts for a row split.
#[derive(Clone, Debug)]
struct RowSplit { parents: u32, undirected: u32, children: u32 }

/// Named index ranges for a row inside `col_index`.
#[derive(Clone, Debug)]
struct RowSegments { parents: Range<usize>, undirected: Range<usize>, children: Range<usize> }


impl CaugiGraph {
    pub fn from_csr(
        row_index: Vec<u32>,
        col_index: Vec<u32>,
        etype: Vec<u8>,
        side: Vec<u8>,
        split_index: Vec<(u32,u32,u32)>,
        registry: RegistrySnapshot
    ) -> Result<Self, String> {
        let n = split_index.len();
        if row_index.len() != n+1 { return Err("ROW_INDEX length mismatch".into()); }
        let nnz = *row_index.last().unwrap_or(&0) as usize;
        if col_index.len()!=nnz || etype.len()!=nnz || side.len()!=nnz {
            return Err("NNZ arrays length mismatch".into());
        }
        Ok(Self {
            row_index: row_index.into(),
            col_index: col_index.into(),
            etype: etype.into(),
            side: side.into(),
            split_index: split_index.into(),
            registry
        })
    }

    #[inline]
    fn row_range(&self, i: u32) -> Range<usize> {
        let i = i as usize;
        self.row_index[i] as usize .. self.row_index[i+1] as usize
    }

    #[inline]
    fn row_split(&self, i: u32) -> RowSplit {
        let (p,u,c) = self.split_index[i as usize];
        RowSplit { parents: p, undirected: u, children: c }
    }

    #[inline]
    fn row_segments(&self, i: u32) -> RowSegments {
        let range = self.row_range(i);
        let s = range.start;
        let sp = self.row_split(i);
        let p = sp.parents as usize;
        let u = sp.undirected as usize;
        let c = sp.children as usize;
        RowSegments {
            parents:    s .. s + p,
            undirected: s + p .. s + p + u,
            children:   range.end - c .. range.end,
        }
    }

    /// Parents slice: definite parents only.
    pub fn parents_of(&self, i:u32) -> &[u32] {
        let seg = self.row_segments(i);
        &self.col_index[seg.parents]
    }

    /// Children slice: definite children only.
    pub fn children_of(&self, i:u32) -> &[u32] {
        let seg = self.row_segments(i);
        &self.col_index[seg.children]
    }

    /// Undirected slice: contains undirected and partial edges for both sides.
    pub fn adjacent_undirected_of(&self, i:u32) -> &[u32] {
        let seg = self.row_segments(i);
        &self.col_index[seg.undirected]
    }

    /// Possible parents = definite parents + undirected neighbors that could be parents of i.
    pub fn possible_parents_of(&self, i: u32) -> Vec<u32> {
        use QueryFlags as F;
        let seg = self.row_segments(i);
        let mut out: Vec<u32> = self.col_index[seg.parents.clone()].to_vec();
        for k in seg.undirected {
            let spec = &self.registry.specs[self.etype[k] as usize];
            let s = self.side[k];
            // If this half-edge is at TAIL (s=0), neighbor sits at HEAD -> check HEAD_POSS_PARENT.
            // If this half-edge is at HEAD (s=1), neighbor sits at TAIL -> check TAIL_POSS_PARENT.
            let ok = (s == 0 && spec.flags.contains(F::HEAD_POSS_PARENT))
                || (s == 1 && spec.flags.contains(F::TAIL_POSS_PARENT));
            if ok { out.push(self.col_index[k]); }
        }
        out
    }

    /// Possible children = definite children + undirected neighbors that could be children of i.
    pub fn possible_children_of(&self, i: u32) -> Vec<u32> {
        use QueryFlags as F;
        let seg = self.row_segments(i);
        let mut out: Vec<u32> = self.col_index[seg.children.clone()].to_vec();
        for k in seg.undirected {
            let spec = &self.registry.specs[self.etype[k] as usize];
            let s = self.side[k];
            // If this half-edge is at TAIL (s=0), neighbor sits at HEAD -> check HEAD_POSS_CHILD.
            // If this half-edge is at HEAD (s=1), neighbor sits at TAIL -> check TAIL_POSS_CHILD.
            let ok = (s == 0 && spec.flags.contains(F::HEAD_POSS_CHILD))
                || (s == 1 && spec.flags.contains(F::TAIL_POSS_CHILD));
            if ok { out.push(self.col_index[k]); }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use crate::edges::EdgeRegistry;
    #[test]
    fn basic_slices() {
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        // 0 --> 1 ; 0 o-> 2 ; 1 <-> 2
        let mut b = super::builder::GraphBuilder::new_with_registry(3, true, &reg);
        let cdir = reg.code_of("-->").unwrap();
        let coar = reg.code_of("o->").unwrap();
        let cbi = reg.code_of("<->").unwrap();
        b.add_edge(0,1,cdir).unwrap();
        b.add_edge(0,2,coar).unwrap();
        b.add_edge(1,2,cbi).unwrap();
        let g = b.finalize().unwrap();

        assert_eq!(g.parents_of(1), &[0]);
        assert_eq!(g.children_of(0), &[1]);
        assert_eq!(g.adjacent_undirected_of(2), &[0,1]);

        assert_eq!(g.possible_parents_of(2), vec![0]); // o-> head-side
        assert_eq!(g.possible_children_of(0), vec![1, 2]); // o-> tail-side not a definite child
    }
}