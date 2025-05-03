// src/from_dense_matrix_to_csr.cpp

#include <cpp11.hpp>
#include "csr_builder.h"
#include <Rinternals.h>   // for Rf_getAttrib, R_DimSymbol, TYPEOF, INTEGER, LENGTH

using namespace cpp11;

[[cpp11::register]]
writable::list caugi_create_csr_from_dense(SEXP mat_) {
  // 1) validate input is a matrix
  SEXP dims = Rf_getAttrib(mat_, R_DimSymbol);
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
  int* codes = INTEGER(mat_);

  // 3) scan for positive codes and build edge lists
  std::vector<int> from;
  std::vector<int> to;
  std::vector<int> types;

  // worst-case every entry is non-zero
  int total = n_nodes * n_nodes;
  from.reserve(total);
  to.  reserve(total);
  types.reserve(total);

  // For symmetric codes (bidirected or undirected),
  // only emit one edge per unordered pair, unless only one direction is present.
  auto is_symmetric_code = [&](int code) {
    return code == 2 || code == 5 || code == 6;
  };

  for (int col = 0; col < ncol; ++col) {
    int base = col * nrow;  // column-major offset for mat[,col]
    for (int row = 0; row < nrow; ++row) {
      int code = codes[base + row];        // mat[row, col]
      if (code <= 0) continue;             // zero means no edge

      if (is_symmetric_code(code)) {
        // check reverse direction mat[col, row]
        int rev_code = codes[row * nrow + col];
        if (rev_code > 0) {
          // mutual: emit only once when row < col
          if (row < col) {
            from.push_back(row + 1);
            to.  push_back(col + 1);
            types.push_back(code);
          }
        } else {
          // single-sided undirected: emit regardless
          from.push_back(row + 1);
          to.  push_back(col + 1);
          types.push_back(code);
        }
      } else {
        // directed or mixed codes: emit every edge
        from.push_back(row + 1);
        to.  push_back(col + 1);
        types.push_back(code);
      }
    }
  }

  // 4) delegate to the pure-C++ builder core
  caugi::CSR csr = caugi::build_csr(n_nodes, from, to, types);

  // 5) copy back into R vectors
  writable::integers row_ptr(   csr.row_ptr.begin(),    csr.row_ptr.end());
  writable::integers col_ids(   csr.col_ids.begin(),    csr.col_ids.end());
  writable::integers type_codes(csr.type_codes.begin(), csr.type_codes.end());

  writable::list out(3);
  out[0] = std::move(row_ptr);
  out[1] = std::move(col_ids);
  out[2] = std::move(type_codes);
  out.names() = {"row_ptr", "col_ids", "type_codes"};
  return out;
}
