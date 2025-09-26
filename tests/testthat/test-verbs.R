test_that("build() generic dispatches", {
  expect_error(build(1), "Can't find method")
})

test_that("build.caugi_graph builds with and without edges", {
  cg <- caugi_graph()
  cg <- add_nodes(cg, name = c("A", "B"))
  cg0 <- build(cg) # no edges
  expect_true(cg0@built)
  expect_false(is.null(cg0@ptr))

  cg <- add_edges(cg, from = "A", edge = "-->", to = "B")
  cg1 <- build(cg) # with edges
  expect_true(cg1@built)
  expect_equal(cg1@nodes$name, c("A", "B"))
  expect_equal(cg1@edges, tibble::tibble(from = "A", edge = "-->", to = "B"))

  expect_identical(build(cg1), cg1) # identical if built
})

test_that("add_edges validates inputs and updates graph", {
  cg <- caugi_graph()
  expect_identical(add_edges(cg), cg)

  expect_error(
    add_edges(cg, A %-->% B, from = "A", edge = "-->", to = "B"),
    "Provide expressions"
  )
  expect_error(
    add_edges(cg, from = "A"),
    "`from`, `edge`, `to` must all be supplied"
  )
  expect_error(
    add_edges(cg, from = c("A", "B"), edge = "-->", to = "B"),
    "equal length"
  )

  cg1 <- add_edges(cg, from = "A", edge = "-->", to = "B")
  expect_false(cg1@built)
  expect_setequal(cg1@nodes$name, c("A", "B"))
  expect_equal(cg1@edges, tibble::tibble(from = "A", edge = "-->", to = "B"))

  cg2 <- add_edges(cg1,
    from = c("A", "A"),
    edge = c("-->", "-->"),
    to = c("B", "B")
  )
  expect_equal(nrow(cg2@edges), 1L)
})

test_that("add_edges makes built = FALSE, build(cg) makes it TRUE (back and forth)", {
  cg <- caugi_graph()

  cg1 <- add_edges(cg, from = "A", edge = "-->", to = "B")
  expect_false(cg1@built)

  cg1_built <- build(cg1)
  expect_true(cg1_built@built)

  cg2 <- add_edges(cg1,
    from = c("A", "A"),
    edge = c("-->", "-->"),
    to = c("B", "B")
  )
  expect_false(cg2@built)
})

test_that("add_edges expression path works", {
  cg <- caugi_graph()
  cg <- add_edges(
    cg,
    A %-->% B,
    B %---% C,
    C %<->% D,
    D %--o% E,
    E %o->% F,
    F %o-o% A
  )
  expect_false(cg@built)
  expect_setequal(cg@nodes$name, c("A", "B", "C", "D", "E", "F"))
  expect_equal(nrow(cg@edges), 6L)
  expect_true(all(c("from", "edge", "to") %in% names(cg@edges)))
  expect_equal(sort(cg@nodes$name), sort(LETTERS[1:6]))
  expect_equal(
    sort(cg@edges$edge),
    sort(c("o->", "--o", "o-o", "-->", "<->", "---"))
  )
})

test_that("build() errors when breaking simple graph assumptions", {
  cg <- caugi_graph()
  cg <- add_edges(cg,
    from = c("A", "A"),
    edge = c("o->", "-->"),
    to = c("B", "B")
  )
  expect_error(build(cg), "parallel")
  cg <- caugi_graph()
  cg <- add_edges(cg,
    from = c("A", "A"),
    edge = c("-->", "<->"),
    to = c("B", "A")
  )
  expect_error(build(cg), "self-loop")
})

test_that("add_edges expression path (DSL) works (also some + notation)", {
  cg <- caugi_graph()
  cg <- add_nodes(cg, A + B)
  cg <- add_edges(cg, A %-->% B + C)
  expect_setequal(cg@nodes$name, c("A", "B", "C"))
  expect_equal(cg@edges, tibble::tibble(
    from = "A",
    edge = c("-->", "-->"),
    to = c("B", "C")
  ))
})

test_that("remove_edges works and keeps other edges", {
  cg <- caugi_graph()
  cg <- add_edges(cg,
    from = c("A", "A"),
    edge = c("-->", "-->"),
    to = c("B", "C")
  )
  expect_identical(remove_edges(cg), cg) # no-op
  expect_error(
    remove_edges(cg, A %-->% B, from = "A", edge = "-->", to = "B"),
    "Provide expressions"
  )

  cg1 <- remove_edges(cg, from = "A", edge = "-->", to = "B")
  expect_false(cg1@built)
  expect_equal(cg1@edges, tibble::tibble(from = "A", edge = "-->", to = "C"))
})

test_that("set_edges replaces any existing edges for pairs", {
  cg <- caugi_graph()
  cg <- add_edges(cg,
    from = c("A", "A"),
    edge = c("o->", "-->"),
    to = c("B", "B")
  )
  cg1 <- set_edges(cg, from = "A", edge = "<->", to = "B")
  expect_false(cg1@built)
  expect_equal(cg1@edges, tibble::tibble(from = "A", edge = "<->", to = "B"))
})

test_that("set_edges errors whwn both vector and expr paths are given", {
  cg <- caugi_graph()
  cg <- add_edges(cg,
    from = c("A", "A"),
    edge = c("o->", "-->"),
    to = c("B", "B")
  )
  expect_error(
    set_edges(cg, A %-->% B, from = "A", edge = "<->", to = "B"),
    "Provide expressions"
  )
})

test_that("set_edges returns cg if nothing given", {
  cg <- caugi_graph(A %-->% B, B %-->% C)
  cg2 <- set_edges(cg)

  expect_equal(cg, cg2)
})

test_that("add_nodes, remove_nodes cover vector and expr paths", {
  cg <- caugi_graph()
  expect_identical(add_nodes(cg), cg)

  cg1 <- add_nodes(cg, name = c("A", "B"))
  expect_false(cg1@built)
  expect_setequal(cg1@nodes$name, c("A", "B"))

  cg2 <- add_nodes(caugi_graph(), A + B + C)
  expect_setequal(cg2@nodes$name, c("A", "B", "C"))

  cg3 <- add_edges(cg1, from = "A", edge = "-->", to = "B")
  cg4 <- remove_nodes(cg3, name = "A")
  expect_false(cg4@built)
  expect_equal(cg4@nodes$name, "B")
  expect_equal(nrow(cg4@edges), 0L)

  cg5 <- add_edges(cg2, A %-->% B, B %-->% C)
  cg6 <- remove_nodes(cg5)
  expect_equal(cg5, cg6) # no-op if no nodes given
})

test_that("subgraph selects nodes and errors with none", {
  cg <- caugi_graph()
  cg <- add_nodes(cg, name = c("A", "B", "C"))
  cg <- add_edges(cg,
    from = c("A", "B"),
    edge = c("-->", "-->"),
    to = c("B", "C")
  )
  expect_error(subgraph(cg), "No nodes specified")

  sg <- subgraph(cg, A + B)
  expect_setequal(sg@nodes$name, c("A", "B"))
  expect_equal(sg@edges, tibble::tibble(from = "A", edge = "-->", to = "B"))
})

test_that(".get_nodes_tibble branches", {
  expect_equal(
    caugi:::.get_nodes_tibble(NULL, list()),
    tibble::tibble(name = character())
  )
  expect_error(
    caugi:::.get_nodes_tibble(
      name = "A",
      calls = list(quote(A))
    ),
    "or `name`, not both"
  )
  res <- caugi:::.get_nodes_tibble(name = c("A", "A", "B"), calls = list())
  expect_setequal(res$name, c("A", "B"))
})

test_that(".get_edges_tibble vector path and error branches", {
  expect_error(caugi:::.get_edges_tibble(
    from = "A",
    edge = NULL,
    to = "B",
    calls = list()
  ))
  expect_error(caugi:::.get_edges_tibble(
    from = c("A", "B"),
    edge = c("-->"),
    to = c("B"),
    calls = list()
  ))

  res <- caugi:::.get_edges_tibble(
    from = c("A", "A"),
    edge = c("-->", "-->"),
    to = c("B", "B"),
    calls = list()
  )
  expect_equal(nrow(res), 2L)
})

test_that(".get_edges_tibble expression path branch", {
  res <- caugi:::.get_edges_tibble(
    from = NULL, edge = NULL, to = NULL,
    calls = list(
      quote(A %-->% B),
      quote(B %---% C)
    )
  )
  expect_equal(nrow(res), 2L)
  expect_equal(res$from, c("A", "B"))
  expect_equal(res$edge, c("-->", "---"))
  expect_equal(res$to, c("B", "C"))
})

test_that(".get_edges_tibble works with empty input", {
  res <- caugi:::.get_edges_tibble(
    from = NULL, edge = NULL, to = NULL,
    calls = list()
  )
  expect_equal(nrow(res), 0L)
})

test_that(".mark_not_built flips flag", {
  cg <- caugi_graph()
  cg <- add_nodes(cg, name = "A")
  cg <- build(cg)
  out <- caugi:::.mark_not_built(cg)
  expect_false(out@built)
})

test_that(".update_caugi_graph add/remove paths and validations", {
  cg <- caugi_graph()
  cg1 <- caugi:::.update_caugi_graph(cg,
    nodes = tibble::tibble(
      name = c("A", "B", "B")
    ),
    action = "add"
  )
  expect_false(cg1@built)
  expect_setequal(cg1@nodes$name, c("A", "B"))

  cg2 <- caugi:::.update_caugi_graph(cg1,
    edges = dplyr::bind_rows(
      tibble::tibble(from = "A", edge = "-->", to = "B"),
      tibble::tibble(from = "A", edge = "-->", to = "B")
    ),
    action = "add"
  )
  expect_equal(nrow(cg2@edges), 1L)
  expect_setequal(cg@nodes$name, c("A", "B"))

  cg3 <- caugi_graph()
  cg3 <- add_nodes(cg3, name = c("A", "B", "C"))
  cg3 <- add_edges(cg3,
    from = c("A", "B", "A"),
    edge = c("-->", "<->", "o->"),
    to = c("B", "C", "C")
  )
  cg4 <- caugi:::.update_caugi_graph(cg3,
    edges = tibble::tibble(
      from = "A",
      to = "B"
    ),
    action = "remove"
  )
  expect_false(any(cg4@edges$from == "A" & cg4@edges$to == "B"))

  expect_error(
    caugi:::.update_caugi_graph(cg4,
      edges = tibble::tibble(from = "B"),
      action = "remove"
    ),
    "include at least `from` and `to`."
  )
})
