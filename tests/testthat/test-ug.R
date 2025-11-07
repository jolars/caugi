# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── UG graph tests ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("UG graph can be created with undirected edges only", {
  cg <- caugi(
    A %---% B,
    B %---% C,
    C %---% D,
    class = "UG"
  )
  expect_s7_class(cg, caugi)
  expect_equal(cg@graph_class, "UG")
  expect_equal(nrow(cg@nodes), 4)
  expect_equal(nrow(cg@edges), 3)
})

test_that("UG graph rejects directed edges", {
  expect_error(
    caugi(
      A %-->% B,
      class = "UG"
    ),
    "undirected"
  )
})

test_that("UG graph rejects bidirected edges", {
  expect_error(
    caugi(
      A %<->% B,
      class = "UG"
    )
  )
})

test_that("UG graph rejects partially directed edges", {
  expect_error(
    caugi(
      A %o->% B,
      class = "UG"
    )
  )
})

test_that("UG graph rejects partially undirected edges", {
  expect_error(
    caugi(
      A %--o% B,
      class = "UG"
    )
  )
})

test_that("UG graph neighbors_of query works", {
  cg <- caugi(
    A %---% B,
    B %---% C,
    B %---% D,
    class = "UG"
  )
  expect_equal(neighbors(cg, "A"), "B")
  expect_equal(sort(neighbors(cg, "B")), c("A", "C", "D"))
  expect_equal(neighbors(cg, "C"), "B")
  expect_equal(neighbors(cg, "D"), "B")
})

test_that("UG graph parents_of and children_of are not defined", {
  cg <- caugi(
    A %---% B,
    class = "UG"
  )
  expect_error(parents(cg, "A"), "not defined for UG")
  expect_error(children(cg, "B"), "not defined for UG")
})

test_that("UG graph ancestors_of and descendants_of return empty", {
  cg <- caugi(
    A %---% B,
    B %---% C,
    class = "UG"
  )
  expect_equal(ancestors(cg, "B"), character(0))
  expect_equal(descendants(cg, "B"), character(0))
})

test_that("UG graph markov_blanket_of returns neighbors", {
  cg <- caugi(
    A %---% B,
    B %---% C,
    B %---% D,
    class = "UG"
  )
  expect_equal(sort(markov_blanket(cg, "B")), c("A", "C", "D"))
  expect_equal(markov_blanket(cg, "A"), "B")
})

test_that("UG graph exogenous_nodes works", {
  cg <- caugi(
    A %---% B,
    B %---% C,
    nodes = c("A", "B", "C", "D", "E"),
    class = "UG"
  )
  expect_equal(sort(exogenous(cg)), c("D", "E"))
})

test_that("UG graph with only isolated nodes", {
  cg <- caugi(
    nodes = c("A", "B", "C"),
    class = "UG"
  )
  expect_equal(nrow(cg@edges), 0)
  expect_equal(nrow(cg@nodes), 3)
  expect_equal(sort(exogenous(cg)), c("A", "B", "C"))
})

test_that("UG graph with only isolated nodes", {
  cg <- caugi(
    nodes = c("A", "B", "C"),
    class = "UG"
  )
  expect_equal(nrow(cg@edges), 0)
  expect_equal(nrow(cg@nodes), 3)
  expect_equal(sort(exogenous_nodes(cg)), c("A", "B", "C"))
})
  # Create a triangle (cycle)
  cg <- caugi(
    A %---% B,
    B %---% C,
    C %---% A,
    class = "UG"
  )
  expect_equal(nrow(cg@nodes), 3)
  expect_equal(nrow(cg@edges), 3)
  
  # Each node should have 2 neighbors
  expect_length(neighbors(cg, "A"), 2)
  expect_length(neighbors(cg, "B"), 2)
  expect_length(neighbors(cg, "C"), 2)
})

test_that("UG graph can create a path", {
  cg <- caugi(
    A %---% B,
    B %---% C,
    C %---% D,
    D %---% E,
    class = "UG"
  )
  expect_equal(neighbors(cg, "A"), "B")
  expect_equal(sort(neighbors(cg, "B")), c("A", "C"))
  expect_equal(sort(neighbors(cg, "C")), c("B", "D"))
  expect_equal(sort(neighbors(cg, "D")), c("C", "E"))
  expect_equal(neighbors(cg, "E"), "D")
})

test_that("UG graph can create a star", {
  cg <- caugi(
    Center %---% A + B + C + D,
    class = "UG"
  )
  expect_equal(sort(neighbors(cg, "Center")), c("A", "B", "C", "D"))
  expect_equal(neighbors(cg, "A"), "Center")
  expect_equal(neighbors(cg, "B"), "Center")
  expect_equal(neighbors(cg, "C"), "Center")
  expect_equal(neighbors(cg, "D"), "Center")
})

test_that("UG graph length returns number of nodes", {
  cg <- caugi(
    A %---% B,
    B %---% C,
    class = "UG"
  )
  expect_equal(length(cg), 3)
  
  cg_empty <- caugi(class = "UG")
  expect_equal(length(cg_empty), 0)
})
