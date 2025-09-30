// SPDX-License-Identifier: MIT
//! Class-agnostic Structural Hamming Distance over CaugiGraph.

use crate::{graph::CaugiGraph, edges::EdgeClass};

#[derive(PartialEq, Eq)]
enum PairKind { None, Sym(EdgeClass), Asym(EdgeClass, bool) }

#[inline]
fn pair_kind(core:&CaugiGraph, v:u32, mut k:usize, end:usize)->(PairKind,usize){
    while k<end {
        let c = core.col_index[k];
        if c < v { k += 1; continue; }
        if c > v { return (PairKind::None, k); }
        let spec = &core.registry.specs[core.etype[k] as usize];
        let kind = if spec.symmetric { PairKind::Sym(spec.class) }
                   else { PairKind::Asym(spec.class, core.side[k]==0) };
        return (kind, k+1);
    }
    (PairKind::None, k)
}

// existing index-based SHD kept as-is:
pub fn shd(t:&CaugiGraph, g:&CaugiGraph)->(f64,usize){ shd_with_perm(t, g, &(0..t.n()).collect::<Vec<_>>()) }

// NEW: SHD using a permutation of g’s indices so that name(i in t) == name(perm[i] in g)
pub fn shd_with_perm(t:&CaugiGraph, g:&CaugiGraph, perm:&[u32]) -> (f64, usize) {
    assert_eq!(t.n(), g.n(), "graph size mismatch");
    assert_eq!(perm.len() as u32, t.n(), "perm length mismatch");
    let n = t.n() as usize;
    if n<=1 { return (0.0, 0); }
    let mut dist = 0usize;
    for u in 0..n {
        let (ts, te) = { let r = t.row_range(u as u32); (r.start, r.end) };
        let u_g = perm[u] as usize;
        let (gs, ge) = { let r = g.row_range(u_g as u32); (r.start, r.end) };
        let (mut kt, mut kg) = (ts, gs);
        for v in (u+1)..n {
            let (a, kt2) = pair_kind(t, v as u32, kt, te);
            let v_g = perm[v];
            let (b, kg2) = pair_kind(g, v_g, kg, ge);
            if a != b { dist += 1; }
            kt = kt2; kg = kg2;
        }
    }
    let comps = n*(n-1)/2;
    ((dist as f64)/(comps as f64), dist)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edges::EdgeRegistry;
    use crate::graph::builder::GraphBuilder;

    #[test]
    fn identical_zero() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let e_dir = reg.code_of("-->").unwrap();

        let mut gb = GraphBuilder::new_with_registry(3, true, &reg);
        gb.add_edge(0, 1, e_dir).unwrap();
        gb.add_edge(0, 2, e_dir).unwrap();
        let core = gb.finalize().unwrap();

        assert_eq!(shd(&core, &core), (0.0, 0));
    }

    #[test]
    fn reversed_direction_counts_one() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let e_dir = reg.code_of("-->").unwrap();

        // truth: 0->1
        let mut gb_t = GraphBuilder::new_with_registry(2, true, &reg);
        gb_t.add_edge(0, 1, e_dir).unwrap();
        let core_t = gb_t.finalize().unwrap();

        // guess: 1->0
        let mut gb_g = GraphBuilder::new_with_registry(2, true, &reg);
        gb_g.add_edge(1, 0, e_dir).unwrap();
        let core_g = gb_g.finalize().unwrap();

        assert_eq!(shd(&core_t, &core_g), (1.0, 1));
    }

    #[test]
    fn extra_edge_counts_once() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let e_dir = reg.code_of("-->").unwrap();

        // truth: 0->1
        let mut gb_t = GraphBuilder::new_with_registry(3, true, &reg);
        gb_t.add_edge(0, 1, e_dir).unwrap();
        let core_t = gb_t.finalize().unwrap();

        // guess: 0->1 plus 0->2
        let mut gb_g = GraphBuilder::new_with_registry(3, true, &reg);
        gb_g.add_edge(0, 1, e_dir).unwrap();
        gb_g.add_edge(0, 2, e_dir).unwrap();
        let core_g = gb_g.finalize().unwrap();

        assert_eq!(shd(&core_t, &core_g), (1.0 / 3.0, 1));
    }

    #[test]
    fn undirected_vs_bidirected_different() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let e_und = reg.code_of("---").unwrap();
        let e_bi = reg.code_of("<->").unwrap();

        // truth: 1---2
        let mut gb_t = GraphBuilder::new_with_registry(3, true, &reg);
        gb_t.add_edge(1, 2, e_und).unwrap();
        let core_t = gb_t.finalize().unwrap();

        // guess: 1<->2
        let mut gb_g = GraphBuilder::new_with_registry(3, true, &reg);
        gb_g.add_edge(1, 2, e_bi).unwrap();
        let core_g = gb_g.finalize().unwrap();

        assert_eq!(shd(&core_t, &core_g), (1.0 / 3.0, 1));
    }

    #[test]
    fn mixed_graph_partial_diffs() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let e_dir = reg.code_of("-->").unwrap();
        let e_und = reg.code_of("---").unwrap();
        let e_bi = reg.code_of("<->").unwrap();

        // truth: 0->1, 1---2
        let mut gb_t = GraphBuilder::new_with_registry(3, true, &reg);
        gb_t.add_edge(0, 1, e_dir).unwrap();
        gb_t.add_edge(1, 2, e_und).unwrap();
        let core_t = gb_t.finalize().unwrap();

        // guess: 0->1, 1<->2
        let mut gb_g = GraphBuilder::new_with_registry(3, true, &reg);
        gb_g.add_edge(0, 1, e_dir).unwrap();
        gb_g.add_edge(1, 2, e_bi).unwrap();
        let core_g = gb_g.finalize().unwrap();

        assert_eq!(shd(&core_t, &core_g), (1.0 / 3.0, 1));
    }

    #[test]
    fn different_number_of_nodes() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let e_dir = reg.code_of("-->").unwrap();

        let mut gb1 = GraphBuilder::new_with_registry(2, true, &reg);
        gb1.add_edge(0, 1, e_dir).unwrap();
        let core1 = gb1.finalize().unwrap();

        let mut gb2 = GraphBuilder::new_with_registry(3, true, &reg);
        gb2.add_edge(0, 1, e_dir).unwrap();
        let core2 = gb2.finalize().unwrap();

        let result = std::panic::catch_unwind(|| shd(&core1, &core2));
        assert!(result.is_err());
    }
   
    #[test]
    fn shd_custom_glyph_opposite_same_direction_is_zero() {
        use crate::edges::{EdgeRegistry, EdgeSpec, Mark, EdgeClass, QueryFlags};
        use crate::graph::builder::GraphBuilder;
        let mut reg = EdgeRegistry::new(); reg.register_builtins().unwrap();
        reg.register(EdgeSpec{ glyph:"<--".into(), tail:Mark::Arrow, head:Mark::Tail,
            symmetric:false, class:EdgeClass::Directed,
            flags:QueryFlags::TRAVERSABLE_WHEN_CONDITIONED }).unwrap();
        let dir = reg.code_of("-->").unwrap();
        let rev = reg.code_of("<--").unwrap();

        // A -> B
        let mut b1 = GraphBuilder::new_with_registry(2, true, &reg);
        b1.add_edge(0,1,dir).unwrap();
        let c1 = b1.finalize().unwrap();

        // B <-- A  (also A -> B)
        let mut b2 = GraphBuilder::new_with_registry(2, true, &reg);
        b2.add_edge(1,0,rev).unwrap();
        let c2 = b2.finalize().unwrap();

        assert_eq!(shd(&c1,&c2), (0.0, 0));
    }

}
