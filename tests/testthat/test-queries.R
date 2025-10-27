# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Queries tests ─────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── caugi type check ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("is_caugi works", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "PDAG")
  expect_true(is_caugi(cg))

  not_cg <- list(a = 1, b = 2)
  expect_false(is_caugi(not_cg))

  expect_error(is_caugi(not_cg, throw_error = TRUE))
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── Acyclicity ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
test_that("acyclicity check works", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %-->% A)

  expect_false(is_acyclic(cg))

  cg <- cg |> set_edges(C %---% A)
  expect_true(is_acyclic(cg))
})

test_that("is_acyclic forces check when requested", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %-->% D, class = "DAG")

  expect_true(is_acyclic(cg))
  expect_true(is_acyclic(cg, force_check = TRUE))
})

test_that("query builds", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "PDAG")
  expect_true(cg@built)

  cg <- cg |> add_edges(D %-->% E)
  expect_false(cg@built)

  expect_true(is_acyclic(cg))

  # now it should be build
  expect_true(cg@built)
})

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Is it <type>? ─────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("is_dag works", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %-->% D, class = "DAG")
  expect_true(is_dag(cg))

  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "PDAG")
  expect_false(is_dag(cg))

  cg <- caugi_graph(A %-->% B, B %-->% C, C %-->% D, class = "PDAG")
  expect_true(is_dag(cg))

  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "Unknown")
  expect_false(is_dag(cg))

  cg <- cg |> set_edges(C %-->% D)
  expect_true(is_dag(cg))
})

test_that("is_pdag works", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "PDAG")
  expect_true(is_pdag(cg))

  cg <- caugi_graph(A %-->% B, B %-->% C, C %-->% D, class = "DAG")
  expect_true(is_pdag(cg))

  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "Unknown")
  expect_true(is_pdag(cg))

  cg <- cg |> set_edges(C %o->% D)
  expect_false(is_pdag(cg))

  cg <- caugi_graph(A %-->% B, B %-->% C, C %o->% D, class = "Unknown")

  expect_false(is_pdag(cg))
})

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Getter queries ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("parents returns expected nodes for names, indices, and expr", {
  cg <- caugi_graph(A %-->% B, B %-->% C, A %---% D, D %-->% B, class = "PDAG")
  expect_identical(parents(cg, "B"), c("A", "D"))
  expect_identical(parents(cg, "B"), c("A", "D"))
  pa_AB <- parents(cg, c("A", "B"))
  expect_null(pa_AB[["A"]])
  expect_setequal(pa_AB[["B"]], c("A", "D"))
  expect_identical(parents(cg, index = 2), c("A", "D"))
  pa_BC <- parents(cg, c("B", "C"))
  expect_setequal(pa_BC[["B"]], c("A", "D"))
  expect_setequal(pa_BC[["C"]], "B")
})

test_that("children returns expected nodes", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, D %-->% E, class = "PDAG")
  expect_identical(children(cg, "A")[[1]], "B")
  expect_identical(children(cg, c("B", "D"))[[1]], "C")
  expect_identical(children(cg, c("C", "D"))[[2]], "E")
  # indices
  expect_identical(children(cg, index = 1)[[1]], "B")
})

test_that("neighbors returns undirected and directed adjacency", {
  cg <- caugi_graph(A %-->% B, B %-->% C, B %---% D, C %---% E, class = "PDAG")
  # B has neighbors A (incoming), C (outgoing), D (undirected)
  expect_setequal(neighbors(cg, "B"), c("A", "C", "D"))
  # C has neighbors B and E
  expect_setequal(neighbors(cg, "C"), c("B", "E"))
})

test_that("queries match with nodes and indexes", {
  cg <- caugi_graph(A %-->% B, B %-->% C, B %---% D, C %---% E, class = "PDAG")
  expect_identical(children(cg, "A"), children(cg, index = 1))
  expect_identical(parents(cg, "B"), parents(cg, index = 2))
  expect_identical(neighbors(cg, "C"), neighbors(cg, index = 3))
})

test_that("queries fail with bad input", {
  cg <- caugi_graph(A %-->% B, B %-->% C, B %---% D, C %---% E, class = "PDAG")
  expect_error(children(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(children(cg), "Supply one of `nodes` or `index`")
  expect_error(parents(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(parents(cg), "Supply one of `nodes` or `index`")
  expect_error(neighbors(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(neighbors(cg), "Supply one of `nodes` or `index`")
  expect_error(ancestors(cg, "Z"), "Non-existant node name: Z")
  expect_error(ancestors(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(ancestors(cg), "Supply one of `nodes` or `index`")
  expect_error(descendants(cg, index = 0), "must be >= 0")
  expect_error(descendants(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(descendants(cg), "Supply one of `nodes` or `index`")
})

test_that("getter queries handle missing relations and duplicates", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  # node with no parents
  expect_identical(length(parents(cg, "A")[[1]]), 0L)
  # node with no children
  expect_identical(length(children(cg, "C")[[1]]), 0L)
  # duplicate targets collapse to the same combined set
  res <- parents(cg, c("B", "B"))
  expect_identical(sort(res[[1]]), sort(parents(cg, "B")[[1]]))
})

test_that("getter queries error on bad nodes or indices", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  expect_error(parents(cg, "Z"), "Non-existant node name: Z")
  expect_error(children(cg, index = 0), "must be >= 0")
  expect_error(children(cg, index = 100), "out of bounds")
})

test_that("aliases route correctly", {
  cg <- caugi_graph(A %-->% B, B %---% C, class = "PDAG")
  expect_identical(neighbours(cg, "B")[[1]], neighbors(cg, "B")[[1]])
})

test_that("public getters trigger lazy build", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "PDAG")
  cg <- cg |> add_edges(C %---% D)
  expect_false(cg@built)
  parents(cg, "B")
  expect_true(cg@built)
})

test_that("nodes and edges getters work", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "PDAG")
  nodes_out <- cg@nodes
  edges_out <- cg@edges

  expect_identical(nodes_out, nodes(cg))
  expect_identical(edges_out, edges(cg))

  expect_identical(nodes_out, vertices(cg))
  expect_identical(nodes_out, V(cg))

  expect_identical(edges_out, E(cg))

  expect_equal(nrow(nodes(cg)), 3L)
  expect_equal(nrow(edges(cg)), 2L)

  cg <- caugi_graph()

  expect_equal(nrow(nodes(cg)), 0L)
  expect_equal(nrow(edges(cg)), 0L)
})

test_that("an and de works", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "PDAG")
  expect_identical(ancestors(cg, "C"), c("A", "B"))
  expect_identical(descendants(cg, "A"), c("B", "C"))
  expect_identical(sort(ancestors(cg, c("C", "D"))[[1]]), c("A", "B"))
  expect_identical(sort(descendants(cg, c("A", "D"))[[1]]), c("B", "C"))

  # test index
  expect_identical(ancestors(cg, index = 3), c("A", "B"))
  expect_identical(descendants(cg, index = 1), c("B", "C"))
  expect_identical(sort(ancestors(cg, index = c(3, 4))[[1]]), c("A", "B"))
  expect_identical(sort(descendants(cg, index = c(1, 4))[[1]]), c("B", "C"))

  cg <- caugi_graph(A, B, class = "DAG")

  expect_equal(length(ancestors(cg, "A")), 0L)
  expect_equal(length(descendants(cg, "A")), 0L)
})

test_that("markov_blanket works on DAGs (parents, children, spouses)", {
  cg <- caugi_graph(
    A %-->% B + C,
    D %-->% B,
    B %-->% E,
    F %-->% E,
    class = "DAG"
  )

  mb_A <- markov_blanket(cg, "A")
  expect_setequal(mb_A, c("B", "C", "D"))

  mb_B <- markov_blanket(cg, "B")
  expect_setequal(mb_B, c("A", "D", "E", "F"))

  # unquoted and vector inputs
  mb_AC <- markov_blanket(cg, c("A", "C"))
  expect_setequal(mb_AC[[1]], c("B", "C", "D"))
  expect_setequal(mb_AC[[2]], c("A"))

  # index input
  mb_idx <- markov_blanket(cg, index = 1)
  expect_setequal(mb_idx, c("B", "C", "D"))
})

test_that("markov_blanket includes undirected neighbors in PDAGs", {
  cg <- caugi_graph(
    A %-->% B,
    B %---% C,
    D %-->% B,
    class = "PDAG"
  )
  mb_B <- markov_blanket(cg, "B")
  expect_setequal(mb_B, c("A", "C", "D"))
})

test_that("markov_blanket argument validation", {
  cg <- caugi_graph(A %-->% B)
  expect_error(markov_blanket(cg), "Supply one of `nodes` or `index`")
  expect_error(
    markov_blanket(cg, nodes = "A", index = 1),
    "either `nodes` or `index`, not both"
  )
})

test_that("exogenous works", {
  cg <- caugi_graph(
    A %-->% B + C,
    D %-->% B,
    B %-->% E,
    F %-->% E,
    class = "DAG"
  )

  e <- exogenous(cg)
  expect_setequal(e, c("A", "D", "F"))

  cg <- caugi_graph(A %---% B, C %-->% A, class = "PDAG")
  e <- exogenous(cg)
  expect_setequal(e, c("B", "C"))

  expect_error(exogenous("not a graph"), "Input must be a caugi_graph")
})

test_that("exogenous works on PDAGs", {
  cg <- caugi_graph(A %---% B, C %-->% D, class = "PDAG")
  e <- exogenous(cg)
  expect_setequal(e, c("A", "B", "C"))

  e <- exogenous(cg, undirected_as_parents = TRUE)
  expect_setequal(e, c("C"))
})

test_that("getter queries errors on non-character input", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  expect_error(parents(cg, 1), "must be a character vector")
  expect_error(children(cg, 2), "must be a character vector")
  expect_error(neighbors(cg, 3), "must be a character vector")
  expect_error(ancestors(cg, 4), "must be a character vector")
  expect_error(descendants(cg, 5), "must be a character vector")
  expect_error(markov_blanket(cg, 6), "must be a character vector")
})

test_that("getter queries builds", {
  getter_queries <- c(parents, children, neighbors, ancestors, descendants, markov_blanket)
  for (getter in getter_queries) {
    cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG", build = FALSE)
    getter(cg, "B")
    expect_true(cg@built)
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Getter helpers ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that(".getter_output returns tibble with name column", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  out <- caugi:::.getter_output(cg, c(0L, 2L), c("A", "C"))
  expect_identical(out[["A"]], "A")
  expect_identical(out[["C"]], "C")

  out_null <- caugi:::.getter_output(cg, 0L, NULL)
  expect_equal(out_null, "A")
})


# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Subgraph ───────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("subgraph selects nodes and errors with none", {
  cg <- caugi_graph()
  cg <- add_nodes(cg, name = c("A", "B", "C"))
  cg <- add_edges(cg,
    from = c("A", "B"),
    edge = c("-->", "-->"),
    to = c("B", "C")
  )
  expect_error(subgraph(cg), "Supply one of `nodes` or `index`.")

  sg <- subgraph(cg, nodes = c("A", "B"))
  expect_setequal(sg@nodes$name, c("A", "B"))
  expect_equal(sg@edges, tibble::tibble(from = "A", edge = "-->", to = "B"))
})
