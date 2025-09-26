# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Queries ────────────────────────────────────
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
  it_is <- inherits(x, "caugi_graph")

  if (throw_error && !it_is) {
    stop("Input must be a caugi_graph", .call = FALSE)
  }
}

#' @title Is the `caugi` graph a DAG?
#'
#' @description Checks if the given `caugi` graph is a
#' Directed Acyclic Graph (DAG).
#'
#' @param cg A `caugi_graph` object.
#'
#' @returns A logical value indicating whether the graph is a DAG.
#' @export
is_dag <- function(cg) {
  is_caugi(cg, throw_error = TRUE)
}
