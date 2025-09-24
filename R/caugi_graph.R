# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── caugi graph API ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Create a `caugi_graph` from edge expressions.
#'
#' @description Create a `caugi_graph` from a series of edge expressions using
#' infix operators. Nodes can be specified as symbols, strings, or numbers.
#'
#' The following edge operators are supported by default:
#' * `%-->%` for directed edges (A --> B)
#' * `%---%` for undirected edges (A --- B)
#' * `%<->%` for bidirected edges (A <-> B)
#' * `%o->%` for PAG directed edges (A o-> B)
#' * `%o--%` for PAG undirected edges (A o-- B)
#' * `%o-o%` for PAG bidirected edges (A o-o B)
#'
#' You can register additional edge types using [register_caugi_edge()].
#'
#' @param ... Edge expressions using the supported infix operators, or
#' nodes given by symbols or strings. Multiple edges can be
#' combined using `+`: `A --> B + C`, indicating an edge from `A` to both `B`
#' and `C`. Nodes can also be grouped using `c(...)` or parentheses.
#' @param simple Logical; if `TRUE` (default), the graph is a simple graph, and
#' the function will throw an error if the input contains parallel edges or
#' self-loops.
#' @param build Logical; if `TRUE` (default), the graph will be built using the
#' Rust backend. If `FALSE`, the graph will not be built, and the Rust backend
#' cannot be used. The graph will build, when queries are made to the graph or
#' if calling `build(cg)`. __Note__: Even if `build = TRUE`, if no edges or
#' nodes are provided, the graph will not be built and the pointer will be
#' `NULL`.
#' @param class Character; one of `"Unknown"`, `"DAG"`, or `"PDAG"`.
#'
#' @returns A `caugi_graph` object containing the nodes, edges, and a pointer
#' to the underlying Rust graph structure.
#'
#' @export
caugi_graph <- function(...,
                        simple = TRUE,
                        build = TRUE,
                        class = c("Unknown", "DAG", "PDAG")) {
  class <- match.arg(class)
  calls <- as.list(substitute(list(...)))[-1L]

  if (!simple && class != "Unknown") {
    stop("If simple = FALSE, class must be 'Unknown'", call. = FALSE)
  }

  # Parse calls into edges + declared nodes
  terms <- .collect_edges_nodes(calls)
  edges <- terms$edges
  declared <- terms$declared

  # All unique nodes
  nodes <- tibble::tibble(name = unique(c(edges$from, edges$to, declared)))
  n <- nrow(nodes)
  id <- setNames(seq_len(n) - 1L, nodes$name)

  # Initialize caugi registry (if not already registered)
  reg <- caugi_registry()

  # Initialize graph pointer. If build = FALSE, the graph will not be built
  # and the pointer will be NULL
  gptr <- NULL

  # If no edges or nodes, do not build the graph
  # Monitor if the graph has been built
  built <- FALSE

  # Build the graph using the Rust backend
  if (build && n > 0L) {
    b <- graph_builder_new(reg, n = n, simple = simple)

    if (nrow(edges)) {
      codes <- vapply(
        edges$edge,
        function(g) edge_registry_code_of(reg, g),
        integer(1L)
      )
      graph_builder_add_edges(
        b,
        as.integer(unname(id[edges$from])),
        as.integer(unname(id[edges$to])),
        as.integer(codes)
      )
    }

    # build + wrap with class
    gptr <- graph_builder_build_view(b, class)
    built <- TRUE
  }
  edges <- tibble::tibble(
    from = edges$from,
    edge = edges$edge,
    to = edges$to
  ) |> dplyr::arrange(from, to, edge)

  structure(
    list(
      nodes = nodes,
      edges = edges,
      ptr = gptr,
      simple = simple,
      built = built,
      class = class
    ),
    class = "caugi_graph"
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────── Methods for caugi graphs ───────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Length of a `caugi_graph`
#'
#' @description Returns the number of nodes in the graph.
#'
#' @param cg A `caugi_graph` object.
#'
#' @returns An integer representing the number of nodes.
#' @exportS3Method caugi::length
length.caugi_graph <- function(cg) {
  nrow(cg$nodes)
}

#' @title Print a `caugi_graph`
#'
#' @exportS3Method print caugi_graph
print.caugi_graph <- function(cg, ...) {
  print(cg$nodes)
  print(cg$edges)
  invisible(cg)
}

#' @title Return union of two `caugi_graphs`
#'
#' @description Returns a new `caugi_graph` that is the union of two input
#' graphs.
#'
#' @param cg1 A `caugi_graph` object.
#' @param cg2 A `caugi_graph` object.
#'
#' @returns A new `caugi_graph` object representing the union of `cg1` and
#' `cg2`.
#'
#' @exportS3Method union caugi_graph
union.caugi_graph <- function(cg1, cg2) {
  NULL
}
