#!/usr/bin/env Rscript
# Generate shared graph fixtures + query specs consumed by every language runner.
#
# Output: fixtures/{id}.edges    -- tab-separated `from\tto` edge list (DAG)
#         fixtures/spec.json     -- per-fixture query parameters

suppressPackageStartupMessages({
  library(caugi)
  library(data.table)
  library(jsonlite)
})

set.seed(42L)

FIXTURES_DIR <- "fixtures"
dir.create(FIXTURES_DIR, showWarnings = FALSE, recursive = TRUE)

# (n, avg_degree) grid. avg_degree is the target average in-degree (= average
# out-degree) per node; the per-edge probability p = 2 * avg_degree / (n - 1)
# turns that into the parameter Erdős-Rényi sampling expects. Holding avg
# degree constant across n gives sparse graphs whose edge count scales linearly
# with n, which is more intuitive than a scaled probability.
GRID <- list(
  list(n = 100L, avg_degree = 3L),
  list(n = 500L, avg_degree = 3L),
  list(n = 1000L, avg_degree = 3L),
  list(n = 100L, avg_degree = 6L),
  list(n = 500L, avg_degree = 6L),
  list(n = 1000L, avg_degree = 6L)
)

# How many random (X, Y) pairs to try before giving up on finding a non-empty
# backdoor adjustment set for the d-separation benchmark.
DSEP_TRIES <- 50L

write_edges <- function(cg, path) {
  e <- caugi::edges(cg)
  e <- e[edge == "-->", .(from, to)]
  fwrite(e, file = path, sep = "\t", col.names = FALSE, quote = FALSE)
  nrow(e)
}

find_dsep_triple <- function(cg, nodes) {
  for (i in seq_len(DSEP_TRIES)) {
    pair <- sample(nodes, 2L, replace = FALSE)
    z <- tryCatch(
      caugi::adjustment_set(cg, X = pair[1], Y = pair[2], type = "backdoor"),
      error = function(e) NULL
    )
    if (!is.null(z) && length(z) > 0L) {
      return(list(x = pair[1], y = pair[2], z = I(as.character(z))))
    }
  }
  NULL
}

fixtures <- list()
for (cell in GRID) {
  n <- cell$n
  avg_degree <- cell$avg_degree
  # avg in-degree d → p such that p * (n - 1) / 2 = d.
  p_mod <- 2 * avg_degree / (n - 1)
  id <- sprintf("n%d_d%d", n, avg_degree)

  message(sprintf(
    "[generate_fixtures] %s: n=%d, avg_degree=%d, p=%.4f",
    id,
    n,
    avg_degree,
    p_mod
  ))

  cg <- caugi::generate_graph(n = n, p = p_mod, class = "DAG", seed = 42L + n)
  cg <- caugi::build(cg)

  edges_path <- file.path(FIXTURES_DIR, paste0(id, ".edges"))
  n_edges <- write_edges(cg, edges_path)

  nodes <- paste0("V", seq_len(n))
  test_node <- nodes[sample.int(n, 1L)]
  subgraph_nodes <- sort(sample(nodes, max(1L, n %/% 2L)))

  dsep <- find_dsep_triple(cg, nodes)

  fixtures[[length(fixtures) + 1L]] <- list(
    id = id,
    n = n,
    p = p_mod,
    avg_degree = avg_degree,
    n_edges = n_edges,
    edges_file = paste0(id, ".edges"),
    test_node = test_node,
    subgraph_nodes = I(subgraph_nodes),
    dsep = dsep
  )
}

spec <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  caugi_version = as.character(utils::packageVersion("caugi")),
  fixtures = fixtures
)

write_json(
  spec,
  path = file.path(FIXTURES_DIR, "spec.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)

message(sprintf(
  "[generate_fixtures] wrote %d fixtures to %s/",
  length(fixtures),
  FIXTURES_DIR
))
