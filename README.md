
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caugi

<!-- badges: start -->

[![R-CMD-check](https://github.com/frederikfabriciusbjerre/caugi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/frederikfabriciusbjerre/caugi/actions/workflows/R-CMD-check.yaml)
[![R
coverage](https://codecov.io/gh/frederikfabriciusbjerre/caugi/graph/badge.svg?flag=r)](https://app.codecov.io/gh/frederikfabriciusbjerre/caugi?flags%5B%5D=r)
[![Rust
coverage](https://codecov.io/gh/frederikfabriciusbjerre/caugi/graph/badge.svg?flag=rust)](https://app.codecov.io/gh/frederikfabriciusbjerre/caugi?flags%5B%5D=rust)
<!-- badges: end -->

> **Causal Graph Interface (for R)** — a fast, tidy toolbox for
> building, coercing and analysing causal graphs.

*caugi* (pronounced **“corgi”**) wraps a high‑performance Rust core in a
pipe‑friendly R interface. Convert between many graph formats, compose
graphs with expressive infix operators, and run algorithms on large
graphs. *caugi* aims to be the go‑to package for causal graphs in R.

## Installation

You can install the development version of caugi from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("frederikfabriciusbjerre/caugi")

# ... or wait for the first CRAN release
# install.packages("caugi")
```

## Example

The `caugi` syntax is very close to how you would draw a graph on a
whiteboard. Here is a tiny DAG:

``` r
library(caugi)

cg <- caugi_graph(A %-->% B + C,
  B %-->% D,
  C %-->% D,
  class = "DAG"
) # optional, guarantees acyclicity by construction
print(cg)
#> # A tibble: 4 × 1
#>   name 
#>   <chr>
#> 1 A    
#> 2 B    
#> 3 C    
#> 4 D    
#> # A tibble: 4 × 3
#>   from  edge  to   
#>   <chr> <chr> <chr>
#> 1 A     -->   B    
#> 2 A     -->   C    
#> 3 B     -->   D    
#> 4 C     -->   D
```

You can query it:

``` r
neighbors(cg, D)
#> # A tibble: 2 × 1
#>   name 
#>   <chr>
#> 1 B    
#> 2 C
parents(cg, D)
#> # A tibble: 2 × 1
#>   name 
#>   <chr>
#> 1 B    
#> 2 C
children(cg, A)
#> # A tibble: 2 × 1
#>   name 
#>   <chr>
#> 1 B    
#> 2 C
ancestors(cg, D)
#> # A tibble: 3 × 1
#>   name 
#>   <chr>
#> 1 A    
#> 2 B    
#> 3 C
descendants(cg, A)
#> # A tibble: 3 × 1
#>   name 
#>   <chr>
#> 1 B    
#> 2 C    
#> 3 D
```

## Key features

| :rocket: | What | Why it matters |
|----|----|----|
| **Flexible coercion & formats** | `as_caugi()` ingests **igraph**, **graphNEL**, **pcalg** `amat` (CPDAG *and* PAG), and sparse or dense (binary/integer‑coded) matrices. | Re‑use existing data structures; no tedious re‑encoding. |
| **PAG & mixed‑graph support** | Native edge codes for PAGs (`o->`, `o-o`, `--o`) and bidirected/undirected edges. | Analyse outputs of discovery algorithms like FCI or RFCI out‑of‑the‑box. |
| **Readable syntax** | `A %-->% B`, `B %<->% C` … | Write graphs exactly as you draw them on a whiteboard. |
| **Blazing speed** | Core implemented in Rust in a Compressed Sparse Row (CSR) representation. | Millions of edges? No problem. |
| **Frontloading computation** | The CSR format is build to be immutable by design. You can add edges/nodes, but the graph is only rebuilt when you call `build()` or query the graph. | Avoids unnecessary recomputation; keeps your code snappy (and keeps you happy). |
| **S7** | Modern S7 classes and methods. Made to be hard to break by mistake. | Future‑proof, safe, and extensible. |
