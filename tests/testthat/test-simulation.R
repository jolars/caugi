test_that("errors: n invalid", {
  expect_error(generate_graph(0, m = 0), "n must be a single integer > 0")
  expect_error(generate_graph(c(2, 3), m = 1), "n must be a single integer > 0")
})

test_that("errors: supply exactly one of m or p", {
  expect_error(generate_graph(5), "exactly one of m or p")
  expect_error(generate_graph(5, m = 2, p = 0.1), "exactly one of m or p")
})

test_that("errors: p invalid", {
  expect_error(generate_graph(4, p = -0.1), "p must be in \\[0,1\\]")
  expect_error(generate_graph(4, p = 1.1), "p must be in \\[0,1\\]")
  expect_error(generate_graph(4, p = NA_real_), "p must be in \\[0,1\\]")
  expect_error(generate_graph(4, p = c(0.1, 0.2)), "p must be in \\[0,1\\]")
})

test_that("errors: m invalid", {
  n <- 4
  tot <- n * (n - 1) / 2
  expect_error(generate_graph(n, m = -1L), "m must be in 0..")
  expect_error(generate_graph(n, m = tot + 1L), "m must be in 0..")
  expect_error(generate_graph(n, m = c(1L, 2L)), "m must be in 0..")
})

test_that("DAG class with m = 0 yields 0 edges and correct nodes", {
  n <- 5
  g <- generate_graph(n, m = 0L, class = "DAG")
  expect_equal(g@graph_class, "DAG")
  expect_equal(nrow(nodes(g)), n)
  expect_equal(nrow(edges(g)), 0L)
  expect_identical(nodes(g)$name, paste0("V", seq_len(n)))
})

test_that("DAG class with p = 0 yields 0 edges", {
  g <- generate_graph(6, p = 0, class = "DAG")
  expect_identical(nrow(edges(g)), 0L)
  expect_true(all(edge_types(g) %in% character()))
})

test_that("m = tot yields a tournament DAG of size n", {
  n <- 6
  tot <- n * (n - 1) / 2
  set.seed(11)
  g <- generate_graph(n, m = tot, class = "DAG")
  expect_equal(nrow(edges(g)), tot)
  expect_true(all(edge_types(g) %in% "-->"))
  # no duplicate undirected pairs
  ed <- edges(g)
  key <- paste(pmin(ed$from, ed$to), pmax(ed$from, ed$to))
  expect_equal(length(unique(key)), tot)
})

test_that("reproducible with same seed", {
  set.seed(42)
  g1 <- generate_graph(7, m = 8, class = "DAG")
  set.seed(42)
  g2 <- generate_graph(7, m = 8, class = "DAG")
  expect_identical(nodes(g1), nodes(g2))
  expect_identical(
    dplyr::arrange(edges(g1), from, to),
    dplyr::arrange(edges(g2), from, to)
  )
})

test_that("p branch draws edges via rbinom", {
  n <- 7
  tot <- n * (n - 1) / 2
  p <- 0.25
  set.seed(123)
  expected_m <- stats::rbinom(1L, tot, p)
  set.seed(123)
  g <- generate_graph(n, p = p, class = "DAG")
  expect_identical(nrow(edges(g)), expected_m)
})

test_that("CPDAG class returns PDAG class and same nodes", {
  g <- generate_graph(6, m = 5, class = "CPDAG")
  expect_identical(g@graph_class, "PDAG")
  expect_identical(nrow(nodes(g)), 6L)
})
