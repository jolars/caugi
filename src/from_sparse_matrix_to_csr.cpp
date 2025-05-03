// src/from_sparse_matrix_to_csr.cpp

#include <cpp11.hpp>
#include "csr_builder.h"
#include <Rinternals.h>   // for Rf_isS4, Rf_inherits, R_do_slot, Rf_install

using namespace cpp11;

[[cpp11::register]]
writable::list caugi_create_csr_from_sparse(SEXP mat_, bool directed) {
  // 1) validate input
  if (!Rf_isS4(mat_) || !Rf_inherits(mat_, "dgCMatrix")) {
    stop("`mat` must be a dgCMatrix");
  }

  // 2) extract dimensions from the "Dim" slot
  SEXP dims = R_do_slot(mat_, Rf_install("Dim"));
  int nrow = INTEGER(dims)[0];
  int ncol = INTEGER(dims)[1];
  if (nrow != ncol) {
    stop("sparse matrix must be square; got %dx%d", nrow, ncol);
  }
  int n_nodes = nrow;

  // 3) extract CSC slots from the S4 object
  SEXP p_slot = R_do_slot(mat_, Rf_install("p"));  // col pointers, length ncol+1
  SEXP i_slot = R_do_slot(mat_, Rf_install("i"));  // row indices, length nnz
  int* p = INTEGER(p_slot);
  int* i = INTEGER(i_slot);

  int nnz = p[ncol];  // total nonzeros

  // 4) build 1-based edge lists
  std::vector<int> from, to, types;
  from.reserve(nnz);
  to.  reserve(nnz);
  types.reserve(nnz);

  // codes: 1 = "-->", 6 = "---"
  const int directed_code   = 1;
  const int undirected_code = 6;

  for (int col = 0; col < ncol; ++col) {
    for (int idx = p[col]; idx < p[col + 1]; ++idx) {
      int row = i[idx];  // 0-based
      from.push_back(row + 1);
      to.  push_back(col + 1);
      types.push_back(directed ? directed_code
                        : undirected_code);
    }
  }

  // 5) delegate to the pure-C++ builder core
  caugi::CSR csr = caugi::build_csr(n_nodes, from, to, types);

  // 6) copy back into R vectors
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
