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
  it_is <- inherits(x, caugi_graph)

  if (!it_is && throw_error) {
    stop("Input must be a caugi_graph", call. = FALSE)
  }
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
#'
#' @returns A logical value indicating whether the graph is a DAG.
#' @export
is_dag <- function(cg, force_check = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  # todo
}

#' @title Is the `caugi` graph a PDAG?
#'
#' @description Checks if the given `caugi` graph is a
#' Partially Directed Acyclic Graph (PDAG).
#'
#' @param cg A `caugi_graph` object.
#' @returns A logical value indicating whether the graph is a PDAG.
#' @export
is_pdag <- function(cg, test = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  # todo
}
