#' @title Generate a `caugi_graph` using Erdős-Rényi.
#'
#' @description Sample a random DAG or CPDAG using
#' Erdős-Rényi for random graph generation.
#'
#' @param n Integer >= 0. Number of nodes in the graph.
#' @param m Integer in `0, n*(n-1)/2`. Number of edges in the graph. Exactly one
#' of `m` or `p` must be supplied.
#' @param p Numeric in `[0,1]`. Probability of edge creation. Exactly one of
#' `m` or `p` must be supplied.
#' @param mode "DAG" or "CPDAG".
#'
#' @returns The sampled `caugi_graph` object.
#'
#' @family simulation functions
#' @concept simulation
#'
#' @export
generate_graph <- function(n, m = NULL, p = NULL, mode = c("DAG", "CPDAG")) {
  mode <- match.arg(mode)
  n <- as.integer(n)
  if (length(n) != 1L || n <= 0L) {
    stop("n must be a single integer > 0", call. = FALSE)
  }
  if (xor(is.null(m), is.null(p)) == FALSE) {
    stop("Supply exactly one of m or p", call. = FALSE)
  }

  tot <- as.integer(n * (n - 1L) / 2L)

  if (!is.null(p)) {
    if (length(p) != 1L || !is.finite(p) || p < 0 || p > 1) {
      stop("p must be in [0,1]", call. = FALSE)
    }
    m <- as.integer(stats::rbinom(1L, tot, p))
  } else {
    m <- as.integer(m)
    if (length(m) != 1L || m < 0L || m > tot) {
      stop(sprintf("m must be in 0..%d", tot), call. = FALSE)
    }
  }

  reg <- edge_registry_new()
  edge_registry_register_builtins(reg)
  b <- graph_builder_new(reg, n, simple = TRUE)
  code_dir <- as.integer(edge_registry_code_of(reg, "-->"))[1L]

  ord <- sample.int(n) - 1L

  if (m > 0L) {
    ranks <- sample.int(tot, m) - 1L
    from <- integer(m)
    to <- integer(m)
    for (k in seq_len(m)) {
      r <- ranks[[k]]
      i <- 0L
      while (r >= (n - 1L - i)) {
        r <- r - (n - 1L - i)
        i <- i + 1L
      }
      j <- i + 1L + r
      from[k] <- ord[i + 1L]
      to[k] <- ord[j + 1L]
    }
    graph_builder_add_edges(b, from, to, rep.int(code_dir, m))
  }

  ptr <- graph_builder_build_view(b, "DAG")
  if (mode == "CPDAG") ptr <- to_cpdag_ptr(ptr)
  .view_to_caugi_graph(ptr)
}
