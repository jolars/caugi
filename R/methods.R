# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Methods ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Length of a `caugi_graph`
#'
#' @description Returns the number of nodes in the graph.
#'
#' @param cg A `caugi_graph` object.
#'
#' @name length
#' @returns An integer representing the number of nodes.
#' @export
S7::method(length, caugi_graph) <- function(x) {
  nrow(x@nodes)
}

#' Print a `caugi_graph`
#'
#' @param x A `caugi_graph` object.
#' @param ... Not used.
#'
#' @name print
#' @export
S7::method(print, caugi_graph) <- function(x, ...) {
  print(cg@nodes)
  print(cg@edges)
  invisible(cg)
}
