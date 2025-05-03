#include "csr_builder.h"
#include <algorithm>
#include <stdexcept>
#include <string>

namespace caugi {

CSR build_csr(int n_nodes,
              const std::vector<int>& from,
              const std::vector<int>& to,
              const std::vector<int>& types) {
  int n_edges = static_cast<int>(from.size());
  if (static_cast<int>(to.size()) != n_edges ||
      static_cast<int>(types.size()) != n_edges) {
    throw std::invalid_argument("`from`, `to`, and `types` must have the same length");
  }

  CSR csr;
  csr.row_ptr.assign(n_nodes + 1, 0);
  csr.col_ids.resize(n_edges);
  csr.type_codes.resize(n_edges);

  // 1) count degree of each source node
  for (int i = 0; i < n_edges; ++i) {
    int u = from[i] - 1;
    if (u < 0 || u >= n_nodes) {
      throw std::out_of_range("`from` index out of range at edge " + std::to_string(i));
    }
    csr.row_ptr[u + 1]++;
  }

  // 2) prefix-sum to get row_ptr offsets
  for (int k = 1; k <= n_nodes; ++k) {
    csr.row_ptr[k] += csr.row_ptr[k - 1];
  }

  // 3) temp cursor to track next write position per row
  std::vector<int> next_pos(n_nodes);
  for (int k = 0; k < n_nodes; ++k) {
    next_pos[k] = csr.row_ptr[k];
  }

  // 4) fill col_ids and type_codes
  for (int i = 0; i < n_edges; ++i) {
    int u = from[i] - 1;
    int v = to[i]   - 1;
    if (v < 0 || v >= n_nodes) {
      throw std::out_of_range("`to` index out of range at edge " + std::to_string(i));
    }
    int p = next_pos[u]++;
    if (p < csr.row_ptr[u] || p >= csr.row_ptr[u + 1]) {
      throw std::runtime_error("insertion overflow for node " + std::to_string(u));
    }
    // store back as 1-based
    csr.col_ids[p]    = v + 1;
    csr.type_codes[p] = types[i];
  }

  return csr;
}

} // namespace caugi
