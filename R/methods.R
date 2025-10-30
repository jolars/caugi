# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Methods ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Length of a `caugi`
#'
#' @description Returns the number of nodes in the graph.
#'
#' @param x A `caugi` object.
#'
#' @name length
#'
#' @returns An integer representing the number of nodes.
#'
#' @examples
#' cg <- caugi(
#'   A %-->% B,
#'   class = "DAG"
#' )
#' length(cg) # 2
#'
#' cg2 <- caugi(
#'   A %-->% B + C,
#'   nodes = LETTERS[1:5],
#'   class = "DAG"
#' )
#' length(cg2) # 5
#'
#' @family caugi methods
#' @concept methods
#'
#' @export
S7::method(length, caugi) <- function(x) {
  nrow(x@nodes)
}

#' Print a `caugi`
#'
#' @param x A `caugi` object.
#' @param ... Not used.
#'
#' @name print
#'
#' @returns The input `caugi` object, invisibly.
#'
#' @examples
#' cg <- caugi(
#'   A %-->% B + C,
#'   nodes = LETTERS[1:5],
#'   class = "DAG"
#' )
#' print(cg)
#'
#' @family caugi methods
#' @concept methods
#'
#' @export
S7::method(print, caugi) <- function(x, ...) {
  print(x@nodes)
  print(x@edges)
  invisible(x)
}
