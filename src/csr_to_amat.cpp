// src/csr_to_amat.cpp
//
// Convert caugi CSR (row_ptr / col_ids / type_codes) back to the
// integer adjacency matrix used for CPDAGs or PAGs.
//
// ── CPDAG codes ───────────────────────────────────────────────
// 0 = no edge / tail
// 1 = arrowhead
//
// ── PAG codes ─────────────────────────────────────────────────
// 0 = no edge, 1 = circle, 2 = arrowhead, 3 = tail
//
// Edge-type levels (same order as in from_amat_to_csr.cpp)
//   1 = -->   2 = <->   3 = o->   4 = o--   5 = o-o   6 = ---
//
// The CSR stores each logical edge once in its canonical orientation.

#include <cpp11.hpp>
#include <Rinternals.h>
#include <algorithm>
#include <string>

using namespace cpp11;

[[cpp11::register]]
SEXP caugi_create_amat_from_csr(const integers& row_ptr,
                                const integers& col_ids,
                                const integers& type_codes,
                                std::string type) {

  if (row_ptr.size() < 2) {
    stop("`row_ptr` must have length ≥ 2");
  }
  if (col_ids.size() != type_codes.size()) {
    stop("`col_ids` and `type_codes` must have the same length");
  }

  const bool cpdag = (type == "cpdag");
  const bool pag   = (type == "pag");
  if (!cpdag && !pag) {
    stop("`type` must be either \"cpdag\" or \"pag\"");
  }

  const int n_nodes = static_cast<int>(row_ptr.size()) - 1;

  // Allocate n × n integer matrix (column-major like R)
  SEXP mat = PROTECT(Rf_allocMatrix(INTSXP, n_nodes, n_nodes));
  std::fill_n(INTEGER(mat),
              static_cast<R_xlen_t>(n_nodes) * n_nodes, 0);

  // Helper: linear index for (row i, col j)
  auto idx = [n_nodes](int i, int j) -> R_xlen_t {
    return static_cast<R_xlen_t>(i) +
      static_cast<R_xlen_t>(j) * n_nodes;
  };

  for (int u = 0; u < n_nodes; ++u) {
    const int row_start = row_ptr[u];
    const int row_end   = row_ptr[u + 1];

    for (int p = row_start; p < row_end; ++p) {
      const int v    = col_ids[p] - 1;       // 0-based
      const int code = type_codes[p];

      if (v < 0 || v >= n_nodes) {
        stop("`col_ids[%d]` (= %d) is out of range [1,%d]",
             p + 1, col_ids[p], n_nodes);
      }

      int& a_uv = INTEGER(mat)[idx(u, v)];
      int& a_vu = INTEGER(mat)[idx(v, u)];

      if (cpdag) {
        switch (code) {
        case 1:                    // -->
          a_uv = 0;                // tail / no arrow
          a_vu = 1;                // arrowhead
          break;
        case 2:                    // <->  undirected
          a_uv = a_vu = 1;
        case 6:                    // ---  undirected
          a_uv = a_vu = 1;
          break;
          // Any other code shouldn’t appear in CPDAG mode,
          // but we ignore it rather than erroring out.
        default:
          break;
        }
      } else {                     // PAG
        switch (code) {
        case 1:                    // -->
          a_uv = 2; a_vu = 3;
          break;
        case 2:                    // <->  bidirected
          a_uv = a_vu = 2;
          break;
        case 3:                    // o->
          a_uv = 1; a_vu = 2;
          break;
        case 4:                    // o--
          a_uv = 1; a_vu = 3;
          break;
        case 5:                    // o-o
          a_uv = a_vu = 1;
          break;
        case 6:                    // ---
          a_uv = a_vu = 3;
          break;
        default:
          break;                   // unknown → ignore
        }
      }
    }
  }

  UNPROTECT(1);
  return mat;
}
