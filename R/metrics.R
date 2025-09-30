# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Metrics ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── Structural metrics ──────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Structural Hamming Distance
#'
#' @description Compute the Structural Hamming Distance (SHD) between two
#' graphs.
#'
#' @param cg1 A `caugi_graph` object.
#' @param cg2 A `caugi_graph` object.
#' @param normalized Logical; if `TRUE`, returns the normalized SHD.
#'
#' @returns An integer representing the SHD between the two graphs.
#' @export
shd <- function(cg1, cg2, normalized = FALSE) {
  is_caugi(cg1, throw_error = TRUE)
  is_caugi(cg2, throw_error = TRUE)
  same_nodes(cg1, cg2, throw_error = TRUE)
  out_lst <- shd_of_ptrs(cg1@ptr, cg1@nodes$name, cg2@ptr, cg2@nodes$name)
  out <- if (normalized) out_lst$normalized else out_lst$count
  out
}
