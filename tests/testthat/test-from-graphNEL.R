# tests/testthat/test-from-graphNEL.R

library(testthat)
library(graph)
library(caugi)
library(tibble)

test_that("as_caugi.graphNEL dispatches and retains directed edges", {
  # build a simple directed graphNEL: A→B, B→C
  nodes <- c("A", "B", "C")
  edgeL <- list(
    A = list(edges = "B"),
    B = list(edges = "C"),
    C = list(edges = character())
  )
  g_nel <- new("graphNEL", nodes = nodes, edgeL = edgeL, edgemode = "directed")

  g1 <- caugi_from_graphNEL(g_nel, collapse = FALSE)
  g2 <- as_caugi(g_nel)

  for (g in list(g1, g2)) {
    df <- as_tibble(g)
    expect_equal(
      df,
      tibble::tibble(
        from      = c("A", "B"),
        to        = c("B", "C"),
        edge_type = c("-->", "-->")
      )
    )
  }
})

test_that("as_caugi.graphNEL collapses mutual edges into undirected", {
  # undirected graphNEL: A—B represented by A→B and B→A
  nodes <- c("A", "B")
  edgeL <- list(
    A = list(edges = "B"),
    B = list(edges = "A")
  )
  g_nel <- new("graphNEL", nodes = nodes, edgeL = edgeL, edgemode = "directed")

  # default collapse = TRUE
  g <- as_caugi(g_nel, collapse = TRUE)
  df <- as_tibble(g)
  expect_equal(
    df,
    tibble::tibble(
      from      = "A",
      to        = "B",
      edge_type = "---"
    )
  )

  g <- as_caugi(g_nel, collapse = TRUE, collapse_to = "<->")
  df <- as_tibble(g)
  expect_equal(
    df,
    tibble::tibble(
      from      = "A",
      to        = "B",
      edge_type = "<->"
    )
  )

  # collapse = FALSE preserves both directions
  g_nc <- caugi_from_graphNEL(g_nel, collapse = FALSE)
  df_nc <- as_tibble(g_nc)
  expect_equal(
    df_nc,
    tibble::tibble(
      from      = c("A", "B"),
      to        = c("B", "A"),
      edge_type = c("-->", "-->")
    )
  )
})

test_that("as_caugi.graphNEL handles graph with no edges", {
  nodes <- c("A", "B")
  edgeL <- list(
    A = list(),
    B = list()
  )
  g_nel <- new("graphNEL", nodes = nodes, edgeL = edgeL, edgemode = "directed")

  g <- as_caugi(g_nel, collapse = TRUE, collapse_to = "<->")
  df <- as_tibble(g)
  expect_equal(
    df,
    tibble::tibble(
      from      = character(),
      to        = character(),
      edge_type = character()
    )
  )
  expect_equal(
    g$nodes,
    tibble::tibble(
      name = c("A", "B")
    )
  )
})
