# tests/testthat/test-from-dense.R

library(testthat)
library(caugi)

test_that("caugi_from_adj_matrix handles directed adj matrix", {
  m <- matrix(0L, 3, 3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  m["a", "b"] <- 1L
  m["b", "c"] <- 1L

  g <- caugi_from_adj_matrix(m, directed = TRUE)
  df <- as_tibble(g)

  expect_equal(
    df,
    tibble::tibble(
      from      = c("a", "b"),
      to        = c("b", "c"),
      edge_type = c("-->", "-->")
    )
  )
})

test_that("caugi_from_adj_matrix handles simple undirected adj matrix", {
  m <- matrix(0L, 3, 3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  m["a", "b"] <- 1L
  m["b", "c"] <- 1L

  g <- caugi_from_adj_matrix(m, directed = FALSE)
  df <- as_tibble(g)

  expect_equal(
    df,
    tibble::tibble(
      from      = c("a", "b"),
      to        = c("b", "c"),
      edge_type = c("---", "---")
    )
  )
})

test_that("caugi_from_adj_matrix throws error for non-adj matrix", {
  m <- matrix(0L, 4, 4,
    dimnames = list(letters[1:4], letters[1:4])
  )
  m["a", "b"] <- 1L # -->
  m["b", "c"] <- 1L # -->
  m["c", "d"] <- 5L # ---

  expect_error(
    caugi_from_adj_matrix(m),
    "x must be a binary adjacency matrix"
  )
})

test_that("as_caugi.matrix and caugi_from_dense handle simple DAG", {
  m <- matrix(0L, 3, 3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  m["a", "b"] <- 1L
  m["b", "c"] <- 1L

  g1 <- caugi_from_dense(m)
  g2 <- as_caugi(m)

  for (g in list(g1, g2)) {
    df <- as_tibble(g)
    expect_equal(
      df,
      tibble::tibble(
        from      = c("a", "b"),
        to        = c("b", "c"),
        edge_type = c("-->", "-->")
      )
    )
  }
})

test_that("caugi_from_dense handles PAG (mixed 'o' and bidirected)", {
  m <- matrix(0L, 4, 4,
    dimnames = list(letters[1:4], letters[1:4])
  )
  m["a", "b"] <- 3L # o->
  m["b", "c"] <- 5L # o-o
  m["c", "d"] <- 2L # <->

  g <- caugi_from_dense(m)
  df <- as_tibble(g)

  expect_equal(
    df,
    tibble::tibble(
      from      = c("a", "b", "c"),
      to        = c("b", "c", "d"),
      edge_type = c("o->", "o-o", "<->")
    )
  )
})

test_that("caugi_from_dense handles fully mixed graph", {
  m <- matrix(0L, 5, 5,
    dimnames = list(LETTERS[1:5], LETTERS[1:5])
  )
  m["A", "B"] <- 1L # -->
  m["B", "C"] <- 2L # <->
  m["C", "B"] <- 2L # <->
  m["C", "D"] <- 3L # o->
  m["D", "E"] <- 5L # o-o
  m["E", "A"] <- 6L # ---

  g <- caugi_from_dense(m)
  df <- as_tibble(g)

  expect_equal(
    df,
    tibble::tibble(
      from      = c("A", "B", "C", "D", "E"),
      to        = c("B", "C", "D", "E", "A"),
      edge_type = c("-->", "<->", "o->", "o-o", "---")
    )
  )
})

test_that("caugi_from_dense handles zero matrix", {
  m <- matrix(0L, 5, 5,
    dimnames = list(LETTERS[1:5], LETTERS[1:5])
  )
  g <- caugi_from_dense(m)
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
      name = c("A", "B", "C", "D", "E")
    )
  )
})
