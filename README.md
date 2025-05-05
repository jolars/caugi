<img src="man/figures/logo.svg" align="right" height="300" alt="" style="padding-left:10px;padding-top:10px;padding-bottom:10px;background-color:white;" />

<h1 style="border: none; padding-bottom: 0; margin-bottom: 0;">caugi</h1>
> **Causal Graph Interface for R** — a fast, tidyverse‑friendly toolbox for building, coercing and analysing causal graphs.



*caugi* (pronounced **“corgi”**) wraps a high‑performance C++ core in a pipe‑friendly R interface. Convert between many graph formats, compose graphs with expressive infix operators, and run algorithms on large sparse matrices — all without leaving the tidyverse.

---

## Key features

| :rocket:                        | What                                                                                                                                                   | Why it matters                                                            |
| ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------- |
| **Flexible coercion & formats** | `as_caugi()` ingests **igraph**, **graphNEL**, **pcalg** `amat` (CPDAG *and* PAG), sparse or dense (binary/integer‑coded) matrices, and tidy edge lists. | Re‑use existing data structures; no tedious re‑encoding.                  |
| **PAG & mixed‑graph support**   | Native edge codes for PAGs (`o->`, `o-o`, `o--`) and bidirected/undirected edges.                                                                      | Analyse outputs of discovery algorithms like FCI or RFCI out‑of‑the‑box.  |
| **Readable syntax**             | `a %-->% b`, `b %<->% c` …                                                                                                                             | Write graphs exactly as you draw them on a whiteboard.                    |
| **Blazing speed**               | Core Compressed Sparse Row (CSR) representation and algorithms in modern C++.                                                                          | Millions of edges? No problem.                                            |
| **Tidy output**                 | `as_tibble()` gives a tidy edge list that plugs straight into **dplyr**/**tidyr**.                                                                     | Analyse & visualise with your favourite tidy tools.                       |
| **igraph interaction**                  | Use `as_igraph()` and analyse and convert back with `as_caugi()` on directed or undirected graphs.                                                                                                                         | Seamlessly tap into igraph’s ecosystem for directed or undirected graphs. |

## Installation

```r
# dev version from GitHub
install.packages("pak")         # if needed
pak::pak("frederikfabriciusbjerre/caugi")

# …or wait for the first CRAN release
# install.packages("caugi")
```

caugi needs R ≥ 4.2 and a C++17‑capable compiler. See `?caugi::caugi` for full details.

## Quick start

```r
library(caugi)

my_graph <- caugi_graph(
  tibble::tibble(name = letters[1:4]),
  a %-->% b,
  b %<->% c,
  d %---% a
)

print(my_graph)
#> # A caugi_graph with 4 nodes and 3 edges
#>   from to edge_type
#> 1 a    b -->
#> 2 b    c <->
#> 3 a    d ---
```

Coercion works both ways:

```r
ig <- igraph::make_ring(5, directed = TRUE)

cg <- as_caugi(ig)
```

## How it works: compact CSR storage

Internally, every *caugi\_graph* lives in **Compressed Sparse Row (CSR)** form:

```
row_ptr    : int[n_nodes + 1]
col_ids    : int[n_edges]
type_codes : int[n_edges]   # maps 1‑6 → {"-->", "<->", …}
```

## Roadmap

We’re actively working on:

* **d‑separation** queries (`dsep()`).
* **Adjustment‑set identification** (`adjustment_sets()`).
* **Graph distances** — Structural Hamming Distance (SHD), simple Hamming Distance (HD), Adjustmen Identification Distance (AID) and more.
* Additional causal metrics and utilities.

Want to help? Open a discussion!

## Contributing

Pull requests, issues and feature requests are welcome. Please open an issue before large changes.

### Style guide

* tidyverse style; run `styler::style_pkg()` before committing.
* Each PR must pass `devtools::check()` with no errors, warnings, or notes.

## License

MIT © 2025 caugi authors.

