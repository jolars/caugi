// SPDX-License-Identifier: MIT
//! Class-agnostic Structural Hamming Distance over CaugiGraph.

use crate::{edges::EdgeClass, graph::CaugiGraph};

#[derive(PartialEq, Eq)]
enum PairKind {
    None,
    Sym(EdgeClass),
    Asym(EdgeClass, bool),
}

#[inline]
fn pair_kind(core: &CaugiGraph, v: u32, mut k: usize, end: usize) -> (PairKind, usize) {
    while k < end {
        let c = core.col_index[k];
        if c < v {
            k += 1;
            continue;
        }
        if c > v {
            return (PairKind::None, k);
        }
        let spec = &core.registry.specs[core.etype[k] as usize];
        let kind = if spec.symmetric {
            PairKind::Sym(spec.class)
        } else {
            PairKind::Asym(spec.class, core.side[k] == 0)
        };
        return (kind, k + 1);
    }
    (PairKind::None, k)
}

#[inline]
fn pair_any(core: &CaugiGraph, v: u32, mut k: usize, end: usize) -> (bool, usize) {
    while k < end {
        let c = core.col_index[k];
        if c < v {
            k += 1;
            continue;
        }
        if c > v {
            return (false, k);
        }
        return (true, k + 1); // found any half-edge between u and v
    }
    (false, k)
}

pub fn shd(t: &CaugiGraph, g: &CaugiGraph) -> (f64, usize) {
    shd_with_perm(t, g, &(0..t.n()).collect::<Vec<_>>())
}

pub fn shd_with_perm(t: &CaugiGraph, g: &CaugiGraph, perm: &[u32]) -> (f64, usize) {
    assert_eq!(t.n(), g.n(), "graph size mismatch");
    assert_eq!(perm.len() as u32, t.n(), "perm length mismatch");
    let n = t.n() as usize;
    if n <= 1 {
        return (0.0, 0);
    }
    let mut dist = 0usize;
    for u in 0..n {
        let (ts, te) = {
            let r = t.row_range(u as u32);
            (r.start, r.end)
        };
        let u_g = perm[u] as usize;
        let (gs, ge) = {
            let r = g.row_range(u_g as u32);
            (r.start, r.end)
        };
        let (mut kt, mut kg) = (ts, gs);
        for v in (u + 1)..n {
            let (a, kt2) = pair_kind(t, v as u32, kt, te);
            let v_g = perm[v];
            let (b, kg2) = pair_kind(g, v_g, kg, ge);
            if a != b {
                dist += 1;
            }
            kt = kt2;
            kg = kg2;
        }
    }
    let comps = n * (n - 1) / 2;
    ((dist as f64) / (comps as f64), dist)
}

pub fn hd(t: &CaugiGraph, g: &CaugiGraph) -> (f64, usize) {
    assert_eq!(t.n(), g.n(), "graph size mismatch");
    let n = t.n() as usize;
    if n <= 1 {
        return (0.0, 0);
    }
    let mut dist = 0usize;
    for u in 0..n {
        let (ts, te) = {
            let r = t.row_range(u as u32);
            (r.start, r.end)
        };
        let (gs, ge) = {
            let r = g.row_range(u as u32);
            (r.start, r.end)
        };
        let (mut kt, mut kg) = (ts, gs);
        for v in (u + 1)..n {
            let (a, kt2) = pair_any(t, v as u32, kt, te);
            let (b, kg2) = pair_any(g, v as u32, kg, ge);
            if a ^ b {
                dist += 1;
            }
            kt = kt2;
            kg = kg2;
        }
    }
    let comps = n * (n - 1) / 2;
    ((dist as f64) / (comps as f64), dist)
}

#[cfg(feature = "gadjid")]
pub mod aid {
    use crate::edges::EdgeClass;
    use crate::graph::CaugiGraph;
    use crate::graph::{dag::Dag, pdag::Pdag};
    use gadjid::PDAG as GPDAG;

    /// Input for AID functions. Only DAG or PDAG accepted.
    pub enum AidInput<'a> {
        Dag(&'a Dag),
        Pdag(&'a Pdag),
    }

    #[inline]
    fn ensure_same_n(t: &GPDAG, g: &GPDAG) -> Result<(), String> {
        if t.n_nodes != g.n_nodes {
            Err("graph size mismatch".into())
        } else {
            Ok(())
        }
    }

    /// Build a gadjid PDAG. If `inv_guess_to_true` is Some, reindex nodes using that mapping.
    /// `inv_guess_to_true[j] = i` means: guess-index `j` should appear at position `i` in the true order.
    fn to_gadjid_pdag_with_map(
        x: AidInput<'_>,
        inv_guess_to_true: Option<&[usize]>,
    ) -> Result<GPDAG, String> {
        match x {
            AidInput::Dag(d) => {
                let n = d.n() as usize;
                if let Some(inv) = inv_guess_to_true {
                    if inv.len() != n {
                        return Err("index map length does not match graph size".into());
                    }
                }
                let mut a = vec![vec![0i8; n]; n];
                let core: &CaugiGraph = d.core_ref();
                for u in 0..n as u32 {
                    for k in core.row_range(u) {
                        let spec = &core.registry.specs[core.etype[k] as usize];
                        if matches!(spec.class, EdgeClass::Directed) && core.side[k] == 0 {
                            // u -> v in DAG
                            let u0 = u as usize;
                            let v0 = core.col_index[k] as usize;
                            let ur = inv_guess_to_true.map_or(u0, |inv| inv[u0]);
                            let vr = inv_guess_to_true.map_or(v0, |inv| inv[v0]);
                            a[ur][vr] = 1;
                        }
                    }
                }
                Ok(GPDAG::from_row_to_column_vecvec(a))
            }
            AidInput::Pdag(g) => {
                if !g.is_cpdag() {
                    return Err("Expected CPDAG input; got a PDAG that is not a CPDAG".into());
                }
                let n = g.n() as usize;
                if let Some(inv) = inv_guess_to_true {
                    if inv.len() != n {
                        return Err("index map length does not match graph size".into());
                    }
                }
                let mut a = vec![vec![0i8; n]; n];

                // directed edges p -> v
                for v in 0..g.n() {
                    for &p in g.parents_of(v) {
                        let p0 = p as usize;
                        let v0 = v as usize;
                        let pr = inv_guess_to_true.map_or(p0, |inv| inv[p0]);
                        let vr = inv_guess_to_true.map_or(v0, |inv| inv[v0]);
                        a[pr][vr] = 1;
                    }
                }
                // undirected edges v --- w
                for v in 0..g.n() {
                    for &w in g.undirected_of(v) {
                        let i0 = v as usize;
                        let j0 = w as usize;
                        let ir = inv_guess_to_true.map_or(i0, |inv| inv[i0]);
                        let jr = inv_guess_to_true.map_or(j0, |inv| inv[j0]);
                        a[ir][jr] = 2;
                        a[jr][ir] = 2;
                    }
                }
                Ok(GPDAG::from_row_to_column_vecvec(a))
            }
        }
    }

    /// Aligned variants. `inv_guess_to_true[j] = i`.
    pub fn ancestor_aid_align(
        true_g: AidInput<'_>,
        guess_g: AidInput<'_>,
        inv_guess_to_true: &[usize],
    ) -> Result<(f64, usize), String> {
        let t = to_gadjid_pdag_with_map(true_g, None)?;
        let g = to_gadjid_pdag_with_map(guess_g, Some(inv_guess_to_true))?;
        ensure_same_n(&t, &g)?;
        Ok(gadjid::graph_operations::ancestor_aid(&t, &g))
    }
    pub fn oset_aid_align(
        true_g: AidInput<'_>,
        guess_g: AidInput<'_>,
        inv_guess_to_true: &[usize],
    ) -> Result<(f64, usize), String> {
        let t = to_gadjid_pdag_with_map(true_g, None)?;
        let g = to_gadjid_pdag_with_map(guess_g, Some(inv_guess_to_true))?;
        ensure_same_n(&t, &g)?;
        Ok(gadjid::graph_operations::oset_aid(&t, &g))
    }
    pub fn parent_aid_align(
        true_g: AidInput<'_>,
        guess_g: AidInput<'_>,
        inv_guess_to_true: &[usize],
    ) -> Result<(f64, usize), String> {
        let t = to_gadjid_pdag_with_map(true_g, None)?;
        let g = to_gadjid_pdag_with_map(guess_g, Some(inv_guess_to_true))?;
        ensure_same_n(&t, &g)?;
        Ok(gadjid::graph_operations::parent_aid(&t, &g))
    }
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
        use crate::edges::{EdgeClass, EdgeRegistry, EdgeSpec, Mark, QueryFlags};
        use crate::graph::builder::GraphBuilder;
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        reg.register(EdgeSpec {
            glyph: "<--".into(),
            tail: Mark::Arrow,
            head: Mark::Tail,
            symmetric: false,
            class: EdgeClass::Directed,
            flags: QueryFlags::TRAVERSABLE_WHEN_CONDITIONED,
        })
        .unwrap();
        let dir = reg.code_of("-->").unwrap();
        let rev = reg.code_of("<--").unwrap();

        // A -> B
        let mut b1 = GraphBuilder::new_with_registry(2, true, &reg);
        b1.add_edge(0, 1, dir).unwrap();
        let c1 = b1.finalize().unwrap();

        // B <-- A  (also A -> B)
        let mut b2 = GraphBuilder::new_with_registry(2, true, &reg);
        b2.add_edge(1, 0, rev).unwrap();
        let c2 = b2.finalize().unwrap();

        assert_eq!(shd(&c1, &c2), (0.0, 0));
    }

    #[test]
    fn hd_identical_zero() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let mut b = GraphBuilder::new_with_registry(3, true, &reg);
        b.add_edge(0, 1, dir).unwrap();
        b.add_edge(0, 2, dir).unwrap();
        let c = b.finalize().unwrap();
        assert_eq!(hd(&c, &c), (0.0, 0));
    }

    #[test]
    fn hd_direction_ignored() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        // 0->1 vs 1->0 should be HD=0
        let mut bt = GraphBuilder::new_with_registry(2, true, &reg);
        bt.add_edge(0, 1, dir).unwrap();
        let ct = bt.finalize().unwrap();
        let mut bg = GraphBuilder::new_with_registry(2, true, &reg);
        bg.add_edge(1, 0, dir).unwrap();
        let cg = bg.finalize().unwrap();
        assert_eq!(hd(&ct, &cg), (0.0, 0));
    }

    #[test]
    fn hd_undirected_vs_bidirected_ignored() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let und = reg.code_of("---").unwrap();
        let bi = reg.code_of("<->").unwrap();
        let mut bt = GraphBuilder::new_with_registry(3, true, &reg);
        bt.add_edge(1, 2, und).unwrap();
        let ct = bt.finalize().unwrap();
        let mut bg = GraphBuilder::new_with_registry(3, true, &reg);
        bg.add_edge(1, 2, bi).unwrap();
        let cg = bg.finalize().unwrap();
        assert_eq!(hd(&ct, &cg), (0.0, 0));
    }

    #[test]
    fn hd_extra_edge_counts_once() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let mut bt = GraphBuilder::new_with_registry(3, true, &reg);
        bt.add_edge(0, 1, dir).unwrap();
        let ct = bt.finalize().unwrap();
        let mut bg = GraphBuilder::new_with_registry(3, true, &reg);
        bg.add_edge(0, 1, dir).unwrap();
        bg.add_edge(0, 2, dir).unwrap();
        let cg = bg.finalize().unwrap();
        assert_eq!(hd(&ct, &cg), (1.0 / 3.0, 1));
    }

    #[test]
    fn hd_size_mismatch_panics() {
        let mut reg = EdgeRegistry::new();
        reg.register_builtins().unwrap();
        let dir = reg.code_of("-->").unwrap();
        let mut b1 = GraphBuilder::new_with_registry(2, true, &reg);
        b1.add_edge(0, 1, dir).unwrap();
        let c1 = b1.finalize().unwrap();
        let mut b2 = GraphBuilder::new_with_registry(3, true, &reg);
        b2.add_edge(0, 1, dir).unwrap();
        let c2 = b2.finalize().unwrap();
        let res = std::panic::catch_unwind(|| hd(&c1, &c2));
        assert!(res.is_err());
    }
}
