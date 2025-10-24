test_that("cg --> as_igraph --> as_caugi --> as_igraph works for directed", {
  cg <- caugi_graph(
    A %-->% B + C,
    B %-->% D,
    C %-->% D,
    E %-->% F,
    class = "DAG"
  )

  ig <- as_igraph(cg)
  cg2 <- as_caugi(ig, class = "DAG")

  ig2 <- as_igraph(cg2)
  expect_equal(igraph::V(ig)$name, igraph::V(ig2)$name)
  expect_equal(igraph::as_data_frame(ig), igraph::as_data_frame(ig2))
  expect_equal(nodes(cg), nodes(cg2))
  expect_equal(edges(cg), edges(cg2))
})

test_that("errors on non-caugi input", {
  expect_error(as_igraph("not-a-caugi"))
})


test_that("Unknown graph with undirected edges become undirected igraph", {
  cg <- caugi_graph(A %---% B + C,
    C %---% D,
    class = "Unknown"
  )
  ig <- as_igraph(cg)
  expect_false(igraph::is_directed(ig))
})

test_that("Unknown with PAG edges fails", {
  cg <- caugi_graph(A %o-o% B %o->% C, class = "Unknown")
  expect_error(as_igraph(cg))
})

test_that("empty graph handled with given vertices", {
  # Only nodes, no edges
  cg <- caugi_graph(A, B, C, class = "DAG")
  ig <- as_igraph(cg)
  expect_true(all(sort(igraph::V(ig)$name) == sort(nodes(cg)$name)))
  expect_equal(nrow(igraph::as_data_frame(ig)), 0L)
  # Directedness defaults to FALSE when no edge types present
  expect_false(igraph::is_directed(ig))
})

test_that("all undirected edge types yield undirected igraph", {
  cg <- caugi_graph(A %---% B %<->% C, class = "Unknown")
  ig <- as_igraph(cg)
  expect_false(igraph::is_directed(ig))
  ed <- igraph::as_data_frame(ig)
  # undirected collapsed
  expect_true(any(ed$from == "A" & ed$to == "B") || any(ed$from == "B" & ed$to == "A"))
  expect_true(any(ed$from == "B" & ed$to == "C") || any(ed$from == "C" & ed$to == "B"))
  # no duplicates
  expect_equal(nrow(ed), 2L)
})

test_that("all directed edges yield directed igraph", {
  cg <- caugi_graph(
    A %-->% B,
    B %-->% C,
    D %-->% C,
    class = "DAG"
  )
  ig <- as_igraph(cg)
  expect_true(igraph::is_directed(ig))
  ed <- igraph::as_data_frame(ig)
  expect_true(all(ed$from %in% c("A", "B", "D")))
  expect_true(all(ed$to %in% c("B", "C")))
  expect_equal(nrow(ed), 3L)
})

test_that("mixed edges: directed kept, undirected duplicated as bidirected", {
  cg <- caugi_graph(
    A %-->% B,
    B %---% C,
    C %---% D,
    class = "PDAG"
  )
  ig <- as_igraph(cg)
  expect_true(igraph::is_directed(ig))
  ed <- igraph::as_data_frame(ig)
  # A->B remains single
  expect_true(any(ed$from == "A" & ed$to == "B"))
  # B---C becomes B->C and C->B
  expect_true(any(ed$from == "B" & ed$to == "C"))
  expect_true(any(ed$from == "C" & ed$to == "B"))
  # C<->D becomes C->D and D->C
  expect_true(any(ed$from == "C" & ed$to == "D"))
  expect_true(any(ed$from == "D" & ed$to == "C"))
  # total edges: 1 directed + 2*undirected (2 undirected edges) = 5
  expect_equal(nrow(ed), 5L)
})
