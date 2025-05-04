#' caugi: A tidy interface to causal graphs
#'
#' @description
#' **caugi** wraps a high-performance C++17 core in a pipe-friendly R API.
#' It reads and writes many graph formats (edge lists, *igraph*, *graphNEL*,
#' *pcalg* amats, sparse/dense matrices) and stores everything internally in a
#' compact **Compressed Sparse Row (CSR)** structure.  The result is
#' white-board syntax with data-table speed—even for graphs with millions of
#' edges.
#'
#' @details
#' The package is designed for causal discovery and graphical causal
#' inference.  Edge codes cover DAGs, CPDAGs and PAGs; helper functions let you
#' collapse mutual edges, round-trip to *igraph*, and coerce between dense and
#' sparse representations without copy overhead.  Planned extensions include
#' d-separation queries, adjustment-set identification, and distance measures
#' such as SHD/HD.
#'
#' @section Key features:
#' * *Readable construction* — `a %-->% b`, `b %<->% c`, `c %o->% d`, …
#' * *Blazing speed* — core algorithms in modern C++ with CSR caching.
#' * *Format agility* — `as_caugi()` methods for **igraph**, **graphNEL**,
#'   *pcalg* amats (*CPDAG* & *PAG*), sparse/dense matrices, and tidy edge
#'   lists.
#' * *Tidy output* — `as_tibble()` returns a long edge list ready for
#'   **dplyr**/**tidyr** workflows.
#' * *Use igraph* — `as_igraph()` lets you use igraph’s layouts and plotting.
#'
#' @seealso
#' \link[igraph:igraph-package]{igraph},
#' \code{\link{as_caugi}()},
#' \code{\link{as_igraph}()},
#' \code{\link{caugi_graph}()}
#'
#' @author
#' Frederik Fabricius-Bjerre \email{frederik@fabriciusbjerre.dk},
#'
#' @keywords package internal
#' @name caugi-package
"_PACKAGE"

## usethis namespace: start
#' @useDynLib caugi, .registration = TRUE
## usethis namespace: end
NULL
