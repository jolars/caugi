// src/csr.h
#pragma once

#include <vector>
#include <cstdint>

// A simple CSR container, independent of Rcpp/cpp11
struct caugiCSR {
  std::vector<int>     row_ptr;    // size = n_nodes+1, 0-based
  std::vector<int>     col_ids;    // size = n_edges, 0-based
  std::vector<uint8_t> type_codes; // size = n_edges

  caugiCSR(int n_nodes,
           const std::vector<int>& from,
           const std::vector<int>& to,
           const std::vector<uint8_t>& types)
  {
    int m = from.size();
    row_ptr.assign(n_nodes + 1, 0);
    col_ids.resize(m);
    type_codes.resize(m);

    // 1) count degrees
    for (int e = 0; e < m; ++e) row_ptr[from[e]]++;
    // 2) prefix‐sum into row_ptr
    for (int i = 1; i <= n_nodes; ++i) row_ptr[i] += row_ptr[i-1];

    // 3) populate col_ids & type_codes in reverse
    std::vector<int> cur = row_ptr;
    for (int e = m - 1; e >= 0; --e) {
      int u = from[e];
      int dst = --cur[u];
      col_ids[dst]    = to[e];
      type_codes[dst] = types[e];
    }
  }

  // getters return std::vector<int> for wrapping later
  const std::vector<int>& get_row_ptr()   const { return row_ptr; }
  const std::vector<int>& get_col_ids()   const { return col_ids; }
  const std::vector<int>  get_type_codes() const {
    // expand uint8_t → int
    return std::vector<int>(type_codes.begin(), type_codes.end());
  }
};
