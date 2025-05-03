# -------------------------------------------------------------------------
#  Tests for the new arrow-syntax edge helpers and revised caugi_graph()
# -------------------------------------------------------------------------
library(testthat)
library(caugi)
library(tibble)
library(dplyr)

# -------------------------------------------------------------------------
test_that("%-->%, %<->%, %---% build the right edge-spec tibbles", {
  sp1 <- A %-->% B
  sp2 <- B %<->% C
  sp3 <- C %---% D

  ## class & structure
  lapply(list(sp1, sp2, sp3), function(x) {
    expect_s3_class(x, "caugi_edge_spec")
    expect_true(all(c("from", "to", "edge_type") %in% names(x)))
    expect_equal(nrow(x), 1)
  })

  ## correct contents
  expect_identical(sp1$from, "A")
  expect_identical(sp1$to, "B")
  expect_identical(sp1$edge_type, "-->")

  expect_identical(sp2$edge_type, "<->")
  expect_identical(sp3$edge_type, "---")
})

# -------------------------------------------------------------------------
test_that("caugi_graph() with **only** edge-specs infers nodes & edges", {
  g <- caugi_graph(
    A %-->% B,
    B %<->% C,
    C %---% D
  )

  expect_s3_class(g, "caugi_graph")
  expect_setequal(g$nodes$name, c("A", "B", "C", "D"))

  ed <- as_tibble(g)
  expect_equal(nrow(ed), 3)
  expect_setequal(ed$edge_type, c("-->", "<->", "---"))
})

# -------------------------------------------------------------------------
test_that("node-tibble + edge-specs works and validates unknown nodes", {
  nodes <- tibble(name = c("A", "B", "C", "D"))

  ## valid call ------------------------------------------------------------
  g_ok <- caugi_graph(
    nodes,
    A %-->% B,
    C %---% D
  )
  expect_identical(g_ok$nodes, nodes)
  expect_equal(as_tibble(g_ok)$edge_type, c("-->", "---"))

  ## unknown endpoint should error ----------------------------------------
  expect_error(
    caugi_graph(nodes, A %-->% Z)
  )
})

# -------------------------------------------------------------------------
test_that("edge-specs and formulas mix happily", {
  g <- caugi_graph(
    A %-->% B, # edge-spec
    B ~ C # old formula (defaults to "-->")
  )

  ed <- as_tibble(g) |> arrange(from, to)
  expect_equal(nrow(ed), 2)
  expect_true(all(ed$edge_type == "-->"))

  expect_setequal(g$nodes$name, c("A", "B", "C"))
})

# -------------------------------------------------------------------------
test_that("formula-only invocation still works (regression guard)", {
  g <- caugi_graph(
    A ~ B + C, # produces A→B, A→C
    C ~ D # produces C→D
  )
  ed <- as_tibble(g)
  expect_equal(nrow(ed), 3)
  expect_true(all(ed$edge_type == "-->"))
  expect_setequal(g$nodes$name, c("A", "B", "C", "D"))
})

# -------------------------------------------------------------------------
test_that("classic two-tibble signature is unchanged", {
  nodes_df <- tibble(name = c("X", "Y"))
  edges_df <- tibble(from = "X", to = "Y", edge_type = "-->")

  g <- caugi_graph(nodes_df, edges_df)
  expect_true(is.list(g$csr)) # quick sanity for CSR build
  expect_equal(as_tibble(g), edges_df)
})
