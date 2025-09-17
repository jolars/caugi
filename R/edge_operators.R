# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── Infix operators ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Edge specification infix operators
#'
#' @description These infix operators are used to specify edges in
#' `caugi_graph()`. This function helps build infix operators.
#'
#' @param from_sym A symbol representing the source node.
#' @param to_expr An expression representing the target node(s).
#' Can be a symbol, string, number, `c(...)`, or a combination using `+`.
#' @param glyph A string representing the edge glyph (e.g., `"-->"`, `"<->"`).
#'
#' @returns A tibble with columns `from`, `to`, and `edge`.
#'
#' @keywords internal
.edge_spec <- function(from_sym, to_expr, glyph) {
  from <- deparse1(from_sym)
  tos <- .expand_targets(to_expr)
  tibble::tibble(from = from, to = tos, edge = glyph) |>
    structure(class = c("caugi_edge_spec", "tbl_df", "tbl", "data.frame"))
}

#' @title Helper to expand the right-hand side of an edge specification
#'
#' @description This function expands the right-hand side of an edge
#' specification into a character vector of target node names. It handles
#' various forms of input, including symbols, calls with `+`, calls with `c()`,
#' and character literals.
#'
#' @param expr An expression representing the target node(s).
#'
#' @returns A character vector of target node names.
#'
#' @keywords internal
.expand_targets <- function(expr) {
  # Handle symbols/names
  if (is.symbol(expr)) {
    return(deparse1(expr))
  }
  # Handle calls: `A %-->% B + C + D`
  if (is.call(expr) && identical(expr[[1L]], as.name("+"))) {
    return(c(.expand_targets(expr[[2L]]), .expand_targets(expr[[3L]])))
  }
  # Handle c(B, C, D)
  if (is.call(expr) && identical(expr[[1L]], as.name("c"))) {
    args <- as.list(expr)[-1L]
    return(unlist(lapply(args, .expand_targets), use.names = FALSE))
  }
  # Character scalar literal: "B"
  if (is.character(expr) && length(expr) == 1L) {
    return(expr)
  }
  stop("Unsupported right-hand side in edge operator: ", deparse(expr), call. = FALSE)
}

#' @title Infix operators for edge specifications
#'
#' @description These operators are used to specify edges in `caugi_graph()`.
#' Should be used internally in `caugi_graph()` calls.
#'
#' @param lhs The left-hand side node expression.
#' @param rhs The right-hand side node expression.
#'
#' @returns A tibble with columns `from`, `to`, and `edge`.
#'
#' @keywords internal
`%-->%` <- function(lhs, rhs) {
  .edge_spec(substitute(lhs), substitute(rhs), "-->")
}
`%<->%` <- function(lhs, rhs) {
  .edge_spec(substitute(lhs), substitute(rhs), "<->")
}
`%---%` <- function(lhs, rhs) {
  .edge_spec(substitute(lhs), substitute(rhs), "---")
}
`%o-o%` <- function(lhs, rhs) {
  .edge_spec(substitute(lhs), substitute(rhs), "o-o")
}
`%o--%` <- function(lhs, rhs) {
  .edge_spec(substitute(lhs), substitute(rhs), "o--")
}
`%o->%` <- function(lhs, rhs) {
  .edge_spec(substitute(lhs), substitute(rhs), "o->")
}
