#ifndef CAUGI_CSR_BUILDER_H
#define CAUGI_CSR_BUILDER_H

#include <vector>

namespace caugi {

  // A simple POD holding CSR data. Immutable once built.
  struct CSR {
    // length = n_nodes + 1
    std::vector<int>    row_ptr;
    // length = n_edges
    std::vector<int>    col_ids;
    std::vector<int>    type_codes;
  };

  // Build a CSR from 1-based `from`, `to`, and `types` vectors.
  // - n_nodes: number of nodes (must be ≥ max(from, to))
  // - from[i], to[i]: 1-based indices in [1..n_nodes]
  // - types[i]: arbitrary integer code per edge
  // Throws std::out_of_range or std::runtime_error on invalid input.
  CSR build_csr(int n_nodes,
                const std::vector<int>& from,
                const std::vector<int>& to,
                const std::vector<int>& types);

} // namespace caugi

#endif // CAUGI_CSR_BUILDER_H
