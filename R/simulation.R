#' Random caugi graph with exactly m edges (G(n, m)) for DAG or CPDAG
#'
#' @param n Integer >= 0
#' @param m Integer in `0, n*(n-1)/2`
#' @param mode "dag" or "cpdag"
#' @param seed Optional integer seed for reproducibility
#' @return An external pointer to a `GraphView` (DAG or PDAG)
#' @export
sample_gnm <- function(n, m, mode = c("dag", "cpdag"), seed = NULL) {
  mode <- match.arg(mode)
  n <- as.integer(n)
  m <- as.integer(m)
  if (length(n) != 1L || n < 0L) stop("n must be a single integer >= 0", call. = FALSE)
  if (length(m) != 1L || m < 0L) stop("m must be a single integer >= 0", call. = FALSE)
  max_m <- as.integer(n * (n - 1L) / 2L)
  if (m > max_m) stop(sprintf("m must be <= %d for n = %d", max_m, n), call. = FALSE)

  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    set.seed(as.integer(seed))
    on.exit(
      {
        if (old_seed_exists) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
  }

  # Registry and builder
  reg <- edge_registry_new()
  edge_registry_register_builtins(reg)
  b <- graph_builder_new(reg, n, simple = TRUE)
  code_dir <- as.integer(edge_registry_code_of(reg, "-->"))[1L]

  # Random permutation for topological order (0-based IDs)
  if (n > 0L) {
    ord <- sample.int(n) - 1L
  } else {
    ord <- integer()
  }

  # Sample m ordered pairs (i < j) without replacement using rank→pair unranking
  if (m > 0L) {
    total <- as.integer(n * (n - 1L) / 2L) # choose(n,2)
    ranks <- sample.int(total, m) - 1L # 0-based ranks in [0, total)
    from <- integer(m)
    to <- integer(m)
    for (k in seq_len(m)) {
      r <- ranks[[k]]
      i <- 0L
      # block sizes for i are (n-1-i)
      while (r >= (n - 1L - i)) {
        r <- r - (n - 1L - i)
        i <- i + 1L
      }
      j <- i + 1L + r
      # map through permutation to create a DAG edge ord[i] -> ord[j]
      from[k] <- ord[i + 1L]
      to[k] <- ord[j + 1L]
    }
    graph_builder_add_edges(b, from, to, rep.int(code_dir, m))
  }

  # Build view
  ptr <- graph_builder_build_view(b, "DAG")
  if (mode == "cpdag") {
    ptr <- to_cpdag_ptr(ptr)
  }
  .view_to_caugi_graph(ptr)
}
