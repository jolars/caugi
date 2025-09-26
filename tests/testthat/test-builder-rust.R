# ───────────────────────── Rust backend FFI tests ─────────────────────────────

test_that("graph builder works for directed edge in reverse direction", {
  reg <- edge_registry_new()
  edge_registry_register_builtins(reg)
  expect_equal(edge_registry_len(reg), 6L)

  # Custom undirected edge without POSS flags so possible_* stay empty
  code_und <- edge_registry_register(
    reg,
    glyph = "<--",
    tail_mark = "arrow",
    head_mark = "tail",
    class = "directed",
    symmetric = FALSE,
    flags = c("TRAVERSABLE_WHEN_CONDITIONED")
  )
  expect_true(is.integer(code_und) || is.double(code_und))

  b <- graph_builder_new(reg, n = 3L, simple = TRUE)

  # test if reverse directed edge works
  graph_builder_add_edges(
    b,
    from = c(0L),
    to = c(1L),
    etype = c(code_und) # undirected edge in the middle
  )

  gptr <- graph_builder_build_view(b, class = "PDAG")

  expect_identical(parents_of_ptr(gptr, 0L), 1L)
  expect_identical(children_of_ptr(gptr, 1L), 0L)
})

test_that("queries work for DAGs and PDAGs", {
  # DAG EXAMPLE
  cg <- caugi_graph(A %-->% B, class = "DAG")

  expect_identical(parents_of_ptr(cg@ptr, 0L), integer())
  expect_identical(children_of_ptr(cg@ptr, 0L), 1L)
  expect_identical(parents_of_ptr(cg@ptr, 1L), 0L)
  expect_identical(children_of_ptr(cg@ptr, 1L), integer())

  cg <- add_edges(cg, B %-->% C)
  cg <- build(cg)

  expect_identical(parents_of_ptr(cg@ptr, 0L), integer())
  expect_identical(children_of_ptr(cg@ptr, 0L), 1L)
  expect_identical(parents_of_ptr(cg@ptr, 1L), 0L)
  expect_identical(children_of_ptr(cg@ptr, 1L), 2L)
  expect_identical(parents_of_ptr(cg@ptr, 2L), 1L)
  expect_identical(children_of_ptr(cg@ptr, 2L), integer())

  expect_error(undirected_of_ptr(cg@ptr, 0L))

  # PDAG EXAMPLE
  cg <- caugi_graph(A %-->% B, class = "PDAG")
  expect_identical(parents_of_ptr(cg@ptr, 0L), integer())
  expect_identical(children_of_ptr(cg@ptr, 0L), 1L)
  expect_identical(parents_of_ptr(cg@ptr, 1L), 0L)
  expect_identical(children_of_ptr(cg@ptr, 1L), integer())

  cg <- add_edges(cg, B %---% C)
  cg <- build(cg)

  expect_identical(parents_of_ptr(cg@ptr, 0L), integer())
  expect_identical(children_of_ptr(cg@ptr, 0L), 1L)
  expect_identical(undirected_of_ptr(cg@ptr, 1L), 2L)
})

test_that("edge registry seal works", {
  reg <- edge_registry_new()
  edge_registry_register_builtins(reg)
  expect_equal(edge_registry_len(reg), 6L)

  code_und <- edge_registry_register(
    reg,
    glyph = "x-x",
    tail_mark = "other",
    head_mark = "other",
    class = "undirected",
    symmetric = TRUE,
    flags = c("TRAVERSABLE_WHEN_CONDITIONED")
  )
  expect_true(is.integer(code_und) || is.double(code_und))
  expect_equal(edge_registry_len(reg), 7L)

  edge_registry_seal(reg)

  expect_error(edge_registry_register(
    reg,
    glyph = "o-O",
    tail_mark = "other",
    head_mark = "other",
    class = "undirected",
    symmetric = TRUE,
    flags = c("TRAVERSABLE_WHEN_CONDITIONED")
  ))

  expect_equal(edge_registry_code_of(reg, "x-x"), as.integer(code_und))
  expect_error(edge_registry_code_of(reg, "unknown"))
})
