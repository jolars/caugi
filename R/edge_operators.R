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
