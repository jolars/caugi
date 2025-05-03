# -------------------------------------------------------------------------
#  tests/testthat/test-as_caugi_from_igraph.R
# -------------------------------------------------------------------------
#  Covers the igraph  ->  caugi_graph  conversion
# -------------------------------------------------------------------------

library(testthat)
library(caugi)
library(igraph)
library(tibble)
library(dplyr)

# =========================================================================
context("as_caugi.igraph()")

# -------------------------------------------------------------------------
test_that("undirected igraph without edge_type → caugi with '---'", {
  ## igraph::make_graph creates an undirected graph by default
  ig <- make_graph(~ A - -B, simplify = FALSE) # one undirected edge
  cg <- as_caugi(ig)

  expect_s3_class(cg, "caugi_graph")
  expect_false(anyNA(match(c("A", "B"), cg$nodes$name)))

  ed <- as_tibble(cg)
  expect_equal(ed$edge_type, "---")
  # from < to ordering enforced
  expect_equal(ed$from, "A")
  expect_equal(ed$to, "B")
})

# -------------------------------------------------------------------------
test_that("directed igraph (all arrows) → caugi directed edges", {
  ig <- graph_from_data_frame(
    data.frame(from = c("A", "B"), to = c("B", "C")),
    directed = TRUE
  )
  cg <- as_caugi(ig)

  expect_equal(as_tibble(cg)$edge_type, rep("-->", 2))
  expect_setequal(cg$nodes$name, c("A", "B", "C"))
})

# -------------------------------------------------------------------------
test_that("mutual symmetric edges (coded '---') collapse to one row", {
  df <- tribble(
    ~from, ~to, ~edge_type,
    "B", "C", "---",
    "C", "B", "---",
    "A", "B", "-->"
  )
  ig <- graph_from_data_frame(df, directed = TRUE)

  E(ig)$edge_type <- df$edge_type # attach the codes
  cg <- as_caugi(ig)

  ed <- as_tibble(cg) %>% arrange(from, to)
  expect_equal(nrow(ed), 2) # A→B  and  B—C
  expect_true(all(c("-->", "---") %in% ed$edge_type))

  # symmetric edge stored once with canonical (B,C) ordering
  sym <- filter(ed, edge_type == "---")
  expect_equal(sym$from, "B")
  expect_equal(sym$to, "C")
})

# -------------------------------------------------------------------------
test_that("vertex names default to 1:n when missing", {
  ig <- make_empty_graph(3, directed = FALSE) # vertices unnamed
  cg <- as_caugi(ig)

  expect_equal(sort(cg$nodes$name), c("1", "2", "3"))
})

# -------------------------------------------------------------------------
test_that("large random directed igraph survives conversion", {
  set.seed(42)
  n <- 100
  p <- 0.05
  el <- which(matrix(runif(n^2) < p, n, n), arr.ind = TRUE)
  el <- el[el[, 1] != el[, 2], , drop = FALSE] # remove loops
  el <- as.data.frame(el) %>%
    dplyr::arrange(row, col)
  ig <- graph_from_edgelist(
    cbind(
      paste0("V", el[, 1]),
      paste0("V", el[, 2])
    ),
    directed = TRUE
  )
  cg <- as_caugi(ig, collapse = FALSE)

  expect_equal(nrow(as_tibble(cg)), ecount(ig))
  expect_false(any(is.loop(as_igraph(cg))))
})
