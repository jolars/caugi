# ───────────────────────── Rust backend FFI tests ─────────────────────────────

test_that("graph builder roundtrip via FFI works (undirected demo)", {
  # Registry: new + builtins (just to exercise the call) + count
  reg <- edge_registry_new()
  edge_registry_register_builtins(reg)
  expect_equal(edge_registry_len(reg), 6L)

  # Register a custom UND edge so we can obtain its integer code.
  # orientation='none', class='undirected', symmetric=TRUE
  code_und <- edge_registry_register(
    reg,
    glyph = "u-u",
    left_mark = "line",
    right_mark = "line",
    orientation = "none",
    class = "undirected",
    symmetric = TRUE,
    traversable_when_conditioned = TRUE
  )
  expect_true(is.integer(code_und) || is.double(code_und))

  # Builder (n=3 nodes: 0,1,2). simple=TRUE
  b <- graph_builder_new(reg, n = 3L, simple = TRUE)

  # Add edges in bulk: 0--1, 1--2, 1--0 (duplicate opposite dir should be rejected
  # by the builder's duplicate guard since the glyph is symmetric). So just add 0--1, 1--2.
  from <- c(0L, 1L)
  to <- c(1L, 2L)
  et <- rep(as.integer(code_und), length(from))
  graph_builder_add_edges(b, from, to, et)

  # Finalize to graph
  g <- graph_builder_build(b)

  # Queries: for undirected-only graph, parents/children are empty; undirected neighbors match
  expect_identical(undirected_of_ptr(g, 0L), as.integer(c(1L)))
  expect_identical(undirected_of_ptr(g, 1L), as.integer(c(0L, 2L)))
  expect_identical(undirected_of_ptr(g, 2L), as.integer(c(1L)))

  expect_identical(parents_of_ptr(g, 1L), integer())
  expect_identical(children_of_ptr(g, 1L), integer())
  expect_identical(possible_parents_of_ptr(g, 1L), integer())
  expect_identical(possible_children_of_ptr(g, 1L), integer())
})

test_that("single-edge add and basic errors", {
  reg <- edge_registry_new()
  edge_registry_register_builtins(reg)

  code_und <- edge_registry_register(
    reg, "x-x", "line", "line", "none", "undirected", TRUE, TRUE
  )

  b <- graph_builder_new(reg, n = 2L, simple = TRUE)
  graph_builder_add_edge(b, 0L, 1L, as.integer(code_und))

  # Out-of-range node id should error
  expect_error(graph_builder_add_edge(b, 0L, 2L, as.integer(code_und)))

  g <- graph_builder_build(b)
  expect_identical(undirected_of_ptr(g, 0L), as.integer(1L))
  expect_identical(undirected_of_ptr(g, 1L), as.integer(0L))
})
