# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Build a caugi graph ──────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Build the graph now
#' @export
build <- function(cg) UseMethod("build")

#' @title Build the graph now
#'
#' @description If a `caugi_graph` has been modified (nodes or edges added or
#' removed), it is marked as _not built_, i.e `cg$built = FALSE`.
#' This function builds the graph using the Rust backend and updates the
#' internal pointer to the graph. If the graph is already built, it is returned.
#'
#' @param cg A `caugi_graph` object with `cg$built = TRUE`.
#'
#' @export
build.caugi_graph <- function(cg) {
  if (cg$built) {
    return(cg)
  }

  built <- FALSE
  n <- nrow(cg$nodes)
  id <- setNames(seq_len(n) - 1L, cg$nodes$name)

  reg <- caugi_registry()
  b <- graph_builder_new(reg, n = n, simple = cg$simple)

  if (nrow(cg$edges)) {
    codes <- vapply(
      cg$edges$edge,
      function(g) edge_registry_code_of(reg, g),
      integer(1L)
    )
    graph_builder_add_edges(
      b,
      as.integer(unname(id[cg$edges$from])),
      as.integer(unname(id[cg$edges$to])),
      as.integer(codes)
    )
  }

  gptr <- graph_builder_build(b)
  built <- TRUE

  structure(
    list(
      nodes = cg$nodes,
      edges = tibble::tibble(
        from = cg$edges$from,
        edge = cg$edges$edge,
        to = cg$edges$to
      ),
      ptr = gptr,
      simple = cg$simple,
      built = built
    ),
    class = "caugi_graph"
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── Edge verbs ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Caugi graph verbs
#'
#' @title Manipulate nodes and edges of a `caugi_graph`
#' @name caugi_verbs
#' @description Add, remove, or and set nodes or edges to / from a `caugi_graph`
#' object. Edges can be specified using expressions with the infix operators.
#' Alternatively, the edges to be added are specified using the
#' `from`, `edge`, and `to` arguments.
#'
#' @param cg A `caugi_graph` object.
#' @param ... Expressions specifying edges to add using the infix operators,
#' or nodes to add using unquoted names, vectors via `c()`, or `+` composition.
#' @param from Character vector of source node names. Default is `NULL`.
#' @param edge Character vector of edge types. Default is `NULL`.
#' @param to Character vector of target node names. Default is `NULL`.
#' @param name Character vector of node names. Default is `NULL`.
#' @returns The updated `caugi_graph`.
#' @seealso [caugi_graph()], [build()]
NULL

#' @describeIn caugi_verbs Add edges.
#' @export
add_edges <- function(cg, ..., from = NULL, edge = NULL, to = NULL) {
  calls <- as.list(substitute(list(...)))[-1L]
  has_expr <- length(calls) > 0L
  has_vec <- !(is.null(from) && is.null(edge) && is.null(to))
  if (has_expr && has_vec) {
    stop(
      "Provide expressions via the infix operators (`A --> B`) ",
      "or vectors via the `from`, `edge`, and `to` arguments, ",
      "but not both."
    )
  }
  if (!has_expr && !has_vec) {
    return(cg)
  }

  # build edges tibble
  edges <- .get_edges_tibble(from, edge, to, calls)

  # update via helper and return
  .update_caugi_graph(cg, edges = edges, action = "add")
}

#' @describeIn caugi_verbs Remove edges.
#' @export
remove_edges <- function(cg, ..., from = NULL, edge = NULL, to = NULL) {
  calls <- as.list(substitute(list(...)))[-1L]
  has_expr <- length(calls) > 0L
  has_vec <- !(is.null(from) && is.null(edge) && is.null(to))
  if (has_expr && has_vec) {
    stop(
      "Provide expressions via the infix operators (`A --> B`) ",
      "or vectors via the `from`, `edge`, and `to` arguments, ",
      "but not both."
    )
  }
  if (!has_expr && !has_vec) {
    return(cg)
  }

  # build edges tibble
  edges <- .get_edges_tibble(from, edge, to, calls)

  # update via helper and return
  .update_caugi_graph(cg, edges = edges, action = "remove")
}

#' @describeIn caugi_verbs Set edge type for given pair(s).
#' @export
set_edges <- function(cg, ..., from = NULL, edge = NULL, to = NULL) {
  calls <- as.list(substitute(list(...)))[-1L]
  has_expr <- length(calls) > 0L
  has_vec <- !(is.null(from) && is.null(edge) && is.null(to))
  if (has_expr && has_vec) {
    stop(
      "Provide expressions via the infix operators (`A --> B`) ",
      "or vectors via the `from`, `edge`, and `to` arguments, ",
      "but not both."
    )
  }
  if (!has_expr && !has_vec) {
    return(cg)
  }

  edges <- .get_edges_tibble(from, edge, to, calls)

  pairs <- dplyr::distinct(dplyr::select(edges, from, to))
  cg <- .update_caugi_graph(cg, edges = pairs, action = "remove")
  cg <- .update_caugi_graph(cg, edges = edges, action = "add")
  cg
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── Node verbs ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @describeIn caugi_verbs Add nodes.
#' @export
add_nodes <- function(cg, ..., name = NULL) {
  calls <- as.list(substitute(list(...)))[-1L]
  nodes <- .get_nodes_tibble(name, calls)
  if (!nrow(nodes)) {
    return(cg)
  }
  .update_caugi_graph(cg, nodes = nodes, action = "add")
}

#' @describeIn caugi_verbs Remove nodes.
#' @export
remove_nodes <- function(cg, ..., name = NULL) {
  calls <- as.list(substitute(list(...)))[-1L]
  nodes <- .get_nodes_tibble(name, calls)
  if (!nrow(nodes)) {
    return(cg)
  }
  .update_caugi_graph(cg, nodes = nodes, action = "remove")
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Subgraph ───────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Extract a subgraph induced by a set of nodes
#'
#' @description Extract a subgraph induced by a set of nodes.
#'
#' @param cg A `caugi_graph` object.
#' @param ... Unquoted node names, vectors via `c()`, or `+` composition.
#'
#' @returns A `caugi_graph` object containing only the specified nodes and any
#' edges between them.
#'
#' @export
subgraph <- function(cg, ...) {
  calls <- as.list(substitute(list(...)))[-1L]
  nodes <- .get_nodes_tibble(NULL, calls)
  if (!nrow(nodes)) {
    stop("No nodes specified for subgraph.", call. = FALSE)
  }
  drop <- tibble::tibble(name = setdiff(cg$nodes$name, nodes$name))
  if (nrow(drop)) {
    cg <- .update_caugi_graph(cg, nodes = drop, action = "remove")
  }
  cg
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── Internal helpers ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Get nodes tibble from verb call.
#'
#' @description Internal helper to build nodes tibble from verb call.
#'
#' @param name Character vector of node names.
#' @param calls List of calls from `...`.
#'
#' @returns A tibble with column `name` for node names.
#'
#' @keywords internal
.get_nodes_tibble <- function(name, calls) {
  has_vec <- !is.null(name)
  has_expr <- length(calls) > 0L
  if (has_vec && has_expr) {
    stop("Provide nodes via `...` or `name`, not both.", call. = FALSE)
  }
  if (!has_vec && !has_expr) {
    return(tibble::tibble(name = character()))
  }
  nodes <- if (has_vec) {
    as.character(name)
  } else {
    unlist(lapply(calls, .expand_nodes), use.names = FALSE)
  }
  tibble::tibble(name = unique(as.character(nodes)))
}

#' @title Build edges tibble from verb call.
#'
#' @description Internal helper to build edges tibble from verb call.
#'
#' @param from Character vector of source node names.
#' @param edge Character vector of edge types.
#' @param to Character vector of target node names.
#' @param calls List of calls from `...`.
#'
#' @returns A tibble with columns `from`, `edge`, and `to`.
#'
#' @keywords internal
.get_edges_tibble <- function(from, edge, to, calls) {
  has_vec <- !(is.null(from) && is.null(edge) && is.null(to))
  edges <- if (has_vec) {
    if (is.null(from) || is.null(edge) || is.null(to)) {
      stop("`from`, `edge`, `to` must all be supplied.", call. = FALSE)
    }
    if (!(length(from) == length(to) && length(to) == length(edge))) {
      stop("`from`, `edge`, `to` must be equal length.", call. = FALSE)
    }
    tibble::tibble(
      from = as.character(from),
      edge = as.character(edge),
      to   = as.character(to)
    )
  } else {
    if (length(calls) == 0L) {
      return(tibble::tibble(
        from = character(),
        edge = character(),
        to = character()
      ))
    }
    units <- unlist(lapply(calls, .parse_edge_arg), recursive = FALSE)
    x <- .edge_units_to_tibble(units)
    tibble::tibble(
      from = x$from,
      edge = x$edge,
      to   = x$to
    )
  }
  edges
}

#' @title Mark a `caugi_graph` as _not built_.
#'
#' @description When a `caugi_graph` is modified, it should be marked as not
#' built. This function sets the `built` attribute to `FALSE`. Thereby, the Rust
#' backend and the R frontend does not match, and at one point, the
#' `caugi_graph` will need to be rebuild for it to be queried.
#'
#' @param cg A `caugi_graph` object.
#'
#' @returns The same `caugi_graph` object, but with the `built` attribute set to
#' `FALSE`.
#'
#' @keywords internal
.mark_not_built <- function(cg) {
  cg$built <- FALSE
  cg
}

#' @title Update nodes and edges of a `caugi_graph`
#'
#' @description Internal helper to add or remove nodes/edges and mark graph as not built.
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A tibble with column `name` for node names to add/remove.
#' @param edges A tibble with columns `from`, `edge`, `to` for edges to add/remove.
#' @param action One of `"add"` or `"remove"`.
#'
#' @returns The updated `caugi_graph` object.
#' @keywords internal
.update_caugi_graph <- function(cg, nodes = NULL, edges = NULL, action = c("add", "remove")) {
  action <- match.arg(action)

  if (action == "add") {
    if (!is.null(nodes)) {
      cg$nodes <- tibble::tibble(name = unique(c(cg$nodes$name, nodes$name)))
    }
    if (!is.null(edges)) {
      cg$nodes <- tibble::tibble(name = unique(c(
        cg$nodes$name,
        edges$from,
        edges$to
      )))
      cg$edges <- dplyr::distinct(dplyr::bind_rows(cg$edges, edges))
    }
  } else {
    if (!is.null(edges)) {
      keys <- intersect(c("from", "edge", "to"), names(edges))
      if (!all(c("from", "to") %in% keys)) {
        stop("edges must include at least `from` and `to`.")
      }
      edges_key <- dplyr::select(edges, dplyr::all_of(keys))
      cg$edges <- dplyr::anti_join(cg$edges, edges_key, by = keys)
    }
    if (!is.null(nodes)) {
      drop <- nodes$name
      cg$nodes <- tibble::tibble(name = setdiff(cg$nodes$name, drop))
      if (nrow(cg$edges)) {
        cg$edges <- dplyr::filter(cg$edges, !(from %in% drop | to %in% drop))
      }
    }
    cg$nodes <- tibble::tibble(name = unique(cg$nodes$name))
    cg$edges <- dplyr::distinct(cg$edges)
  }
  .mark_not_built(cg)
}
