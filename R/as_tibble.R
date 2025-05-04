#' @title Convert a `caugi_graph` to a tibble.
#'
#' @description Convert a `caugi_graph` object to a tibble.
#' The resulting tibble contains the edges of the graph, with columns for the
#' source node, target node, and edge type.
#'
#' @param x A `caugi_graph` object.
#' @param collapse Logical. If `TRUE`, the edges are collapsed to a symmetric form.
#' @param collapse_to Character. The string used to represent collapsed edges.
#' @export
as_tibble.caugi_graph <- function(x, collapse = FALSE, collapse_to = "---", ...) {
  df <- caugi_edges_df(x)
  if (collapse) df <- collapse_directed_to_symmetric_edges(df, collapse_to)
  df
}

# Helper: raw edges tibble
caugi_edges_df <- function(x) {
  rp <- as.integer(x$csr$row_ptr)
  cols <- as.integer(x$csr$col_ids)
  type_codes <- as.integer(x$csr$type_codes)
  from_idx <- rep(seq_along(diff(rp)), diff(rp))
  check_edge_integer(type_codes)
  tibble(
    from      = x$nodes$name[from_idx],
    to        = x$nodes$name[cols],
    edge_type = edge_type_levels[type_codes]
  )
}
