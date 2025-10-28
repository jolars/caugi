# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Methods ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Length of a `caugi_graph`
#'
#' @description Returns the number of nodes in the graph.
#'
#' @param x A `caugi_graph` object.
#'
#' @name length
#'
#' @returns An integer representing the number of nodes.
#'
#' @examples
#' cg <- caugi_graph(
#'   A %-->% B,
#'   class = "DAG"
#' )
#' length(cg) # 2
#'
#' cg2 <- caugi_graph(
#'   A %-->% B + C,
#'   nodes = LETTERS[1:5],
#'   class = "DAG"
#' )
#' length(cg2) # 5
#'
#' @family caugi_graph methods
#' @concept methods
#'
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
#'
#' @returns The input `caugi_graph` object, invisibly.
#'
#' @examples
#' cg <- caugi_graph(
#'   A %-->% B + C,
#'   nodes = LETTERS[1:5],
#'   class = "DAG"
#' )
#' print(cg)
#'
#' @family caugi_graph methods
#' @concept methods
#'
#' @export
S7::method(print, caugi_graph) <- function(x, ...) {
  print(x@nodes)
  print(x@edges)
  invisible(x)
}
