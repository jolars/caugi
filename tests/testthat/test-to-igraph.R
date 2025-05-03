# -------------------------------------------------------------------------
#  Tests for as_igraph.caugi_graph()
# -------------------------------------------------------------------------
library(testthat)
library(caugi)
library(igraph)
library(dplyr)
library(tibble)

edges_tbl <- function(g) as_tibble(g, collapse = FALSE)

# =========================================================================
context("as_igraph.caugi_graph")

# -------------------------------------------------------------------------
test_that("purely directed caugi → directed igraph", {
  g_caugi <- caugi_graph(
    A %-->% B,
    B %-->% C
  )

  g_ig <- as_igraph(g_caugi)

  expect_true(is.directed(g_ig))
  expect_equal(gorder(g_ig), 3)
  expect_equal(gsize(g_ig), 2)
  expect_true(all(E(g_ig)$edge_type == "-->"))
})

# -------------------------------------------------------------------------
test_that("purely undirected caugi → undirected igraph", {
  g_caugi <- caugi_graph(
    A %---% B,
    C %<->% D
  )

  g_ig <- as_igraph(g_caugi)

  expect_false(is.directed(g_ig))
  expect_equal(gorder(g_ig), 4)
  expect_equal(gsize(g_ig), 2)
  expect_setequal(E(g_ig)$edge_type, c("---", "<->"))
})

# -------------------------------------------------------------------------
test_that("mixed directed + undirected → directed igraph with duplicated symmetric edges", {
  g_caugi <- caugi_graph(
    A %-->% B, # one directed
    B %---% C # one symmetric  ⇒  duplicated
  )
  # Expected edges:  A→B,  B→C,  C→B   (total 3)

  g_ig <- as_igraph(g_caugi)

  expect_true(is.directed(g_ig))
  expect_equal(gsize(g_ig), 3)

  # counts per edge_type
  tbl <- table(E(g_ig)$edge_type)
  expect_equal(unname(tbl["-->"]), 1)
  expect_equal(unname(tbl["---"]), 2)

  # symmetric edge present both ways
  expect_true(are_adjacent(g_ig, "B", "C"))
  expect_true(are_adjacent(g_ig, "C", "B"))
})

# -------------------------------------------------------------------------
test_that("PAG-type edges trigger an error", {
  nodes <- tibble(name = c("X", "Y"))
  edges <- tibble(from = "X", to = "Y", edge_type = "o->")
  g_bad <- caugi_graph(nodes, edges)

  expect_error(
    as_igraph(g_bad),
    regexp = "PAG-type edges"
  )
})
