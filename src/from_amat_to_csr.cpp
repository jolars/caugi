// src/from_amat_to_csr.cpp

#include <cpp11.hpp>
#include "csr_builder.h"
#include <Rinternals.h>   // Rf_getAttrib, R_DimSymbol, TYPEOF, INTEGER, INTSXP

using namespace cpp11;

[[cpp11::register]]
writable::list caugi_create_csr_from_amat(SEXP mat_, std::string type) { SEXP dims = Rf_getAttrib(mat_, R_DimSymbol);
  if (dims == R_NilValue || LENGTH(dims) != 2) {
    stop("`mat` must be a matrix");
  }
  int nrow = INTEGER(dims)[0];
  int ncol = INTEGER(dims)[1];
  if (nrow != ncol) {
    stop("dense matrix must be square; got %dx%d", nrow, ncol);
  }
  int n_nodes = nrow;

  // 2) require integer storage for codes
  if (TYPEOF(mat_) != INTSXP) {
    stop("`mat` must be an integer matrix of edge-type codes");
  }
  int n = nrow;
  int* amat = INTEGER(mat_);

  bool cpdag = (type == "cpdag");
  bool pag   = (type == "pag");
  if (!cpdag && !pag) {
    stop("`type` must be either \"cpdag\" or \"pag\"");
  }

  // 2) Prepare buffers
  std::vector<int> from, to, types;
  from.reserve(n*(n-1)/2);
  to.reserve(n*(n-1)/2);
  types.reserve(n*(n-1)/2);

  // 3) Decode each unordered pair (i < j)
  //    edge_type_levels:
  //      1 = "-->"
  //      2 = "<->"
  //      3 = "o->"
  //      4 = "o--"
  //      5 = "o-o"
  //      6 = "---"
  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      // column-major: amat[col*n + row] == mat[row, col]
      int a = amat[j * n + i];  // mat[i, j]
      int b = amat[i * n + j];  // mat[j, i]
      int code = 0;
      bool swap_ij = false;

      if (cpdag) {
        // CPDAG uses 0 = no edge/tail, 1 = arrowhead
        if (a == 0 && b == 1) {
          code = 1;            // -->
        } else if (a == 1 && b == 0) {
          code = 1;            // <--   to  --> after swap
          swap_ij = true;
        } else if (a == 1 && b == 1) {
          code = 6;            // ---  undirected
        }
      } else {
        // PAG uses 0=no edge, 1=circle, 2=arrowhead, 3=tail (mark refers to column)
        if (a == 2 && b == 3) {
          code = 1;            // -->
        } else if (a == 3 && b == 2) {
          code = 1;            // <--  to  --> after swap
          swap_ij = true;
        } else if (a == 2 && b == 2) {
          code = 2;            // <->  bidirected
        } else if (a == 1 && b == 2) {
          code = 3;            // o->  circle to arrow
        } else if (a == 2 && b == 1) {
          code = 3;            // <-o  to  o-> after swap
          swap_ij = true;
        } else if (a == 1 && b == 3) {
          code = 4;            // o--  circle–tail
        } else if (a == 3 && b == 1) {
          code = 4;            // tail–circle  to  o-- after swap
          swap_ij = true;
        } else if (a == 1 && b == 1) {
          code = 5;            // o-o  circle both ends
        } else if (a == 3 && b == 3) {
          code = 6;            // ---  undirected
        }
      }

      if (code > 0) {
        int u = swap_ij ? j : i;
        int v = swap_ij ? i : j;
        from.push_back(u + 1);
        to.push_back(v + 1);
        types.push_back(code);
      }
    }
  }

  // 4) Build CSR via core builder
  caugi::CSR csr = caugi::build_csr(n, from, to, types);

  // 5) Return to R
  writable::integers row_ptr(csr.row_ptr.begin(), csr.row_ptr.end());
  writable::integers col_ids(csr.col_ids.begin(), csr.col_ids.end());
  writable::integers type_codes(csr.type_codes.begin(), csr.type_codes.end());

  writable::list out(3);
  out[0] = std::move(row_ptr);
  out[1] = std::move(col_ids);
  out[2] = std::move(type_codes);
  out.names() = {"row_ptr", "col_ids", "type_codes"};
  return out;
}
