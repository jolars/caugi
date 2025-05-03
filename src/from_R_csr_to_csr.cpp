#include <cpp11.hpp>
#include "csr_builder.h"

using namespace cpp11;

[[cpp11::register]]
writable::list caugi_create_csr_from_csr(integers from,
                                integers to,
                                integers types,
                                integers n_nodes_) {
  int n_nodes = n_nodes_[0];

  // copy into std::vector<int>
  std::vector<int> from_vec(from.begin(), from.end());
  std::vector<int> to_vec(to.begin(),     to.end());
  std::vector<int> types_vec(types.begin(), types.end());

  // call the pure-C++ builder
  caugi::CSR csr = caugi::build_csr(n_nodes, from_vec, to_vec, types_vec);

  // move back into R integer vectors
  writable::integers row_ptr(csr.row_ptr.begin(),     csr.row_ptr.end());
  writable::integers col_ids(csr.col_ids.begin(),     csr.col_ids.end());
  writable::integers type_codes(csr.type_codes.begin(), csr.type_codes.end());

  writable::list out(3);
  out[0] = std::move(row_ptr);
  out[1] = std::move(col_ids);
  out[2] = std::move(type_codes);
  out.names() = {"row_ptr", "col_ids", "type_codes"};
  return out;
}
