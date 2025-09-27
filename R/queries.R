# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Queries ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────────── Checks ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Is it a `caugi` graph?
#'
#' @description Checks if the given object is a `caugi_graph`.
#'
#' @param x An object to check.
#' @param throw_error Logical; if `TRUE`, throws an error if `x` is not a
#' `caugi_graph`.
#'
#' @returns A logical value indicating whether the object is a `caugi_graph`.
#' @export
is_caugi <- function(x, throw_error = FALSE) {
  it_is <- inherits(x, caugi_graph)

  if (!it_is && throw_error) {
    stop("Input must be a caugi_graph", call. = FALSE)
  }
  it_is
}

#' @title Is the `caugi` acyclic?
#'
#' @description Checks if the given `caugi` graph is acyclic.
#'
#' @param cg A `caugi_graph` object.
#' @param force_check Logical; if `TRUE`, the function will test if the graph is
#' acyclic, if `FALSE` (default), it will look at the graph class.
#'
#' @details
#' __Lazy building__
#'
#' If the graph is not built, the graph will be built before making the query.
#'
#' __Check logic__
#'
#' By the construction of the `caugi_graph`, if the graph class is "DAG" or
#' "PDAG", the graph should be guaranteed to be acyclic. If the graph class is
#' "Unknown", the graph may or may not be acyclic. If `force_check = TRUE`, the
#' function will check if the graph is acyclic using the Rust backend, no matter
#' the class. If the graph class is not inherently acyclic, `is_acyclic` will
#' run the check.
#'
#' __Note__
#'
#' We do not think that it is possible to introduce a cycle in a `caugi_graph`
#' on acyclic classes without the package throwing errors. We might be wrong,
#' though, so `force_check` is e a feature that is there for peace of mind.
#'
#'
#' @returns A logical value indicating whether the graph is acyclic.
#' @export
is_acyclic <- function(cg, force_check = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  if (force_check) {
    is_it <- is_acyclic_ptr(cg@ptr)
  } else if (identical(cg@graph_class, "DAG") ||
    identical(cg@graph_class, "PDAG")) {
    is_it <- TRUE
  } else {
    is_it <- is_acyclic_ptr(cg@ptr)
  }
  is_it
}

#' @title Is the `caugi` graph a DAG?
#'
#' @description Checks if the given `caugi` graph is a
#' Directed Acyclic Graph (DAG).
#'
#' @param cg A `caugi_graph` object.
#' @param force_check Logical; if `TRUE`, the function will test if the graph is
#' a DAG, if `FALSE` (default), it will look at the graph class and match
#' it, if possible.
#'
#' @returns A logical value indicating whether the graph is a DAG.
#' @export
is_dag <- function(cg, force_check = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  if (identical(cg@graph_class, "DAG") && !force_check) {
    is_it <- TRUE
  } else {
    # if we can't be sure from the class, we check
    is_it <- is_dag_type_ptr(cg@ptr)
  }
  is_it
}

#' @title Is the `caugi` graph a PDAG?
#'
#' @description Checks if the given `caugi` graph is a
#' Partially Directed Acyclic Graph (PDAG).
#'
#' @param cg A `caugi_graph` object.
#' @param force_check Logical; if `TRUE`, the function will test if the graph is
#' a PDAG, if `FALSE` (default), it will look at the graph class and match
#' it, if possible.
#'
#' @returns A logical value indicating whether the graph is a PDAG.
#' @export
is_pdag <- function(cg, force_check = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  if (identical(cg@graph_class, "PDAG") && !force_check) {
    is_it <- TRUE
  } else {
    # if we can't be sure from the class, we check
    is_it <- is_pdag_type_ptr(cg@ptr)
  }
  is_it
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Getters ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Get parents of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names or indices, a (vector of) unquoted
#' node name(s), or an expression combining these with `+` and `c()`.
#'
#' @export
parents <- function(cg, nodes) {
  expr <- substitute(nodes)
  .relations(cg, .capture_nodes_expr(expr, parent.frame()), parents_of_ptr)
}

#' @rdname parents
pa <- parents

#' @title Get children of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names or indices, a (vector of) unquoted
#' node name(s), or an expression combining these with `+` and `c()`.
#'
#' @export
children <- function(cg, nodes) {
  expr <- substitute(nodes)
  .relations(cg, .capture_nodes_expr(expr, parent.frame()), children_of_ptr)
}

#' @rdname children
ch <- children

#' @title Get neighbors of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names or indices, a (vector of) unquoted
#' node name(s), or an expression combining these with `+` and `c()`.
#'
#' @export
neighbors <- function(cg, nodes) {
  expr <- substitute(nodes)
  .relations(cg, .capture_nodes_expr(expr, parent.frame()), neighbors_of_ptr)
}

#' @rdname neighbors
neighbours <- neighbors
#' @rdname neighbors
nb <- neighbors
#' @rdname neighbors
neighborhood <- neighbors
#' @rdname neighbors
neighbourhood <- neighbors


# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Getter helpers ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Capture nodes expression
#'
#' @description Capture and evaluate an expression representing nodes.
#'
#' @param expr An expression representing the target node(s).
#' @param env The environment in which to evaluate the expression.
#'
#' @returns A character vector of target node names.
#'
#' @keywords internal
.capture_nodes_expr <- function(expr, env = parent.frame()) {
  if (.is_node_expr(expr)) {
    .expand_nodes(expr)
  } else {
    eval(expr, env)
  }
}

#' @title Resolve node names or indices to zero-based indices
#'
#' @description Convert node names or one-based indices to zero-based indices.
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names or one-based indices.
#'
#' @returns An integer vector of zero-based node indices.
#'
#' @keywords internal
.resolve_idx <- function(cg, nodes) {
  if (is.numeric(nodes)) {
    ix <- as.integer(nodes)
    if (any(ix < 1L)) stop("Indices must be >= 1", call. = FALSE)
    return(ix - 1L)
  }
  nm <- cg@nodes$name
  if (is.null(nm)) stop("Node names unavailable on this graph.", call. = FALSE)
  m <- match(as.character(nodes), nm)
  if (anyNA(m)) {
    bad <- nodes[is.na(m)]
    stop("Unknown node(s): ", paste(bad, collapse = ", "), call. = FALSE)
  }
  m - 1L
}

#' @title Output object of getter queries
#'
#' @description Helper to format the output of getter queries.
#'
#' @param cg A `caugi_graph` object.
#' @param idx0 A vector of zero-based node indices.
#'
#' @returns A tibble with a `name` column.
#'
#' @keywords internal
.getter_output <- function(cg, idx0) {
  id <- as.integer(idx0) + 1L
  nm <- cg@nodes$name
  tibble::tibble(name = if (is.null(nm)) as.character(id) else nm[id])
}

#' @title Get relations of nodes in a `caugi_graph`
#'
#' @description Helper to get relations (parents, children, etc.) of nodes.
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names or indices, a (vector of) unquoted
#' node name(s), or an expression combining these with `+` and `c()`.
#' @param getter A function that takes a pointer to the graph and a zero-based
#' node index, and returns a vector of zero-based indices of related nodes.
#'
#' @returns A tibble with a `name` column.
#'
#' @keywords internal
.relations <- function(cg, nodes, getter) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  idx0 <- .resolve_idx(cg, nodes)
  rows <- lapply(idx0, function(i0) .getter_output(cg, getter(cg@ptr, i0)))
  if (length(rows) == 1L) {
    rows[[1]]
  } else {
    do.call(rbind, rows)
  }
}
