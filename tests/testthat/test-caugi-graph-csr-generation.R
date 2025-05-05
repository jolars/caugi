# tests/testthat/test-caugi-generation.R

library(testthat)
library(caugi)
library(tibble)
library(rlang)
library(dplyr)
library(tidyr)

test_that("Two two-tibble input produces expected caugi_graph", {
  nodes_in <- tibble(name = c("A", "B"))
  edges_in <- tibble(
    from = "A",
    to = "B",
    edge_type = "---"
  )
  g1 <- caugi_graph(nodes_in, edges_in)
  edges_in2 <- tibble(
    from = "B",
    to = "A",
    edge_type = "---"
  )
  g2 <- caugi_graph(nodes_in, edges_in2)

  expect_equal(g1, g2)
})

test_that("Two-tibble input produces expected caugi_graph", {
  nodes_in <- tibble(name = c("A", "B"))
  edges_in <- tibble(
    from = "A",
    to = "B",
    edge_type = "-->"
  )
  g <- caugi_graph(nodes_in, edges_in)

  # Basic structure
  expect_s3_class(g, "caugi_graph")
  expect_named(g, c("nodes", "csr"))

  # Nodes preserved
  expect_equal(g$nodes$name, c("A", "B"))

  # CSR has correct dimensions for one edge
  expect_length(g$csr$row_ptr, nrow(nodes_in) + 1)
  expect_equal(g$csr$row_ptr, c(1L, 2L, 2L))
  expect_equal(g$csr$col_ids, 2L)
  expect_equal(g$csr$type_codes, 1L)
})

test_that("Flexible edge-spec path infers nodes correctly", {
  # single edge spec
  g1 <- caugi_graph(A %-->% B)
  expect_s3_class(g1, "caugi_graph")
  expect_equal(g1$nodes$name, c("A", "B"))
  expect_equal(g1$csr$col_ids, 2L)

  # two edge specs, reverse order of appearance
  g2 <- caugi_graph(C %---% A, B %<->% C)
  # nodes inferred in order unique(c("C","A","B","C")) -> c("C","A","B")
  expect_equal(g2$nodes$name, c("C", "A", "B"))

  # type codes match the order of edge_type_levels
  codes <- factor(c("---", "<->"), levels = edge_type_levels)
  expect_equal(g2$csr$type_codes, as.integer(codes))
})

test_that("Formula path crosses lhs and rhs correctly", {
  # A ~ B + C gives two edges A->B and A->C
  g <- caugi_graph(A ~ B + C)
  expect_s3_class(g, "caugi_graph")
  # nodes unique(c("A","B","C"))
  expect_equal(g$nodes$name, c("A", "B", "C"))

  # Check that there are two outgoing edges from A
  # row_ptr: starts at 1, then cumulative counts
  # with 3 nodes, expect length 4
  expect_length(g$csr$row_ptr, 4)
  # col_ids should contain B and C (IDs 2 and 3)
  expect_setequal(g$csr$col_ids, c(2L, 3L))
})

test_that("Edge-spec operators produce correct caugi_edge_spec objects", {
  ops <- list(
    `%-->%` = "-->",
    `%<->%` = "<->",
    `%---%` = "---",
    `%o--%` = "o--",
    `%o->%` = "o->",
    `%o-o%` = "o-o"
  )

  for (op in names(ops)) {
    lhs <- substitute(X)
    rhs <- substitute(Y)
    edge <- eval(call(op, quote(X), quote(Y)))
    expect_s3_class(edge, "caugi_edge_spec")
    expect_equal(edge$from, "X")
    expect_equal(edge$to, "Y")
    expect_equal(edge$edge_type, ops[[op]])
  }
})

test_that("Errors are thrown for invalid inputs", {
  # Unknown node in two-tibble path
  expect_error(
    caugi_graph(tibble(name = "A"), B %-->% C)
  )

  # Formula without both sides
  expect_error(
    caugi_graph(~A)
  )

  # Non-formula/non-edge-spec after optional node tibble
  expect_error(
    caugi_graph(tibble(name = "A"), 123)
  )

  # from/to must match nodes$name
  bad_nodes <- tibble(name = c("X", "Y"))
  bad_edges <- tibble(from = "A", to = "B", edge_type = "-->")
  expect_error(
    caugi_graph(bad_nodes, bad_edges)
  )
})
