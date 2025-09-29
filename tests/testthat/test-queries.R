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
  # by name
  expect_identical(sort(parents(cg, "B")$name), c("A", "D"))
  # by unquoted expression
  expect_identical(parents(cg, B)$name, c("A", "D"))

  # by unquoted expression
  expect_identical(sort(parents(cg, A + B)$name), c("A", "D"))
  # vector of names
  res <- parents(cg, c("B", "C"))
  expect_true(all(res$name %in% c("A", "B", "D")))
})

test_that("children returns expected nodes", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, D %-->% E, class = "PDAG")
  expect_identical(children(cg, "A")$name, "B")
  expect_identical(sort(children(cg, c("B", "D"))$name), c("C", "E"))
  # indices
  expect_identical(children(cg, index = 1)$name, "B")
})

test_that("neighbors returns undirected and directed adjacency", {
  cg <- caugi_graph(A %-->% B, B %-->% C, B %---% D, C %---% E, class = "PDAG")
  # B has neighbors A (incoming), C (outgoing), D (undirected)
  expect_identical(sort(neighbors(cg, "B")$name), c("A", "C", "D"))
  # C has neighbors B and E
  expect_identical(sort(neighbors(cg, "C")$name), c("B", "E"))
})

test_that("queries match with nodes and indexes", {
  cg <- caugi_graph(A %-->% B, B %-->% C, B %---% D, C %---% E, class = "PDAG")
  expect_identical(ch(cg, A), ch(cg, index = 1))
  expect_identical(pa(cg, B), pa(cg, index = 2))
  expect_identical(nb(cg, C), nb(cg, index = 3))
})

test_that("queries fail with bad input", {
  cg <- caugi_graph(A %-->% B, B %-->% C, B %---% D, C %---% E, class = "PDAG")
  expect_error(ch(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(ch(cg), "Supply one of `nodes` or `index`")
  expect_error(pa(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(pa(cg), "Supply one of `nodes` or `index`")
  expect_error(nb(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(nb(cg), "Supply one of `nodes` or `index`")
  expect_error(an(cg, "Z"), "Unknown node")
  expect_error(an(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(an(cg), "Supply one of `nodes` or `index`")
  expect_error(de(cg, index = 0), "out of bounds")
  expect_error(de(cg, A, index = 1), "Supply either `nodes` or `index`")
  expect_error(de(cg), "Supply one of `nodes` or `index`")
})

test_that("getter queries handle missing relations and duplicates", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  # node with no parents
  expect_identical(nrow(parents(cg, "A")), 0L)
  # node with no children
  expect_identical(nrow(children(cg, "C")), 0L)
  # duplicate targets return concatenated rows
  res <- parents(cg, c("B", "B"))
  expect_true(nrow(res) >= 2L) # two blocks for the two queries
})

test_that("getter queries error on bad nodes or indices", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  expect_error(parents(cg, "Z"), "Unknown node")
  expect_error(children(cg, index = 0), "out of bounds")
  expect_error(children(cg, index = 100), "out of bounds")
})

test_that("aliases pa/ch/nb/nbhd/neighbours route correctly", {
  cg <- caugi_graph(A %-->% B, B %---% C, class = "PDAG")
  expect_identical(pa(cg, "B")$name, parents(cg, "B")$name)
  expect_identical(ch(cg, "A")$name, children(cg, "A")$name)
  expect_identical(nb(cg, "B")$name, neighbors(cg, "B")$name)
  expect_identical(neighbours(cg, "B")$name, neighbors(cg, "B")$name)
  expect_identical(neighborhood(cg, "B")$name, neighbors(cg, "B")$name)
  expect_identical(neighbourhood(cg, "B")$name, neighbors(cg, "B")$name)
})

test_that("public getters trigger lazy build", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "PDAG")
  cg <- cg |> add_edges(C %---% D)
  expect_false(cg@built)
  expect_s3_class(parents(cg, "B"), "tbl_df")
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
  expect_identical(an(cg, "C")$name, c("A", "B"))
  expect_identical(de(cg, "A")$name, c("B", "C"))
  expect_identical(sort(an(cg, c("C", "D"))$name), c("A", "B"))
  expect_identical(sort(de(cg, c("A", "D"))$name), c("B", "C"))

  # test index
  expect_identical(an(cg, index = 3)$name, c("A", "B"))
  expect_identical(de(cg, index = 1)$name, c("B", "C"))
  expect_identical(sort(an(cg, index = c(3, 4))$name), c("A", "B"))
  expect_identical(sort(de(cg, index = c(1, 4))$name), c("B", "C"))

  cg <- caugi_graph(A, B, class = "DAG")

  expect_equal(nrow(an(cg, "A")), 0L)
  expect_equal(nrow(de(cg, "A")), 0L)
})

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Getter helpers ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that(".resolve_idx maps names and 1-based indices to 0-based", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  # names
  expect_identical(caugi:::.resolve_idx(cg, c("A", "C")), c(0L, 2L))
  # numeric 1-based
  expect_identical(caugi:::.resolve_idx_from_index(cg, c(1, 3)), c(0L, 2L))
  # mixed numeric as integer
  expect_identical(caugi:::.resolve_idx_from_index(cg, 2L), 1L)
  # bad index
  expect_error(caugi:::.resolve_idx_from_index(cg, 0), "out of bounds")
  expect_error(caugi:::.resolve_idx_from_index(cg, "A"), "must be numeric")
  # unknown names
  expect_error(caugi:::.resolve_idx(cg, c("A", "Z")), "Unknown node\\(s\\)")
})

test_that(".getter_output returns tibble with name column", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  out <- caugi:::.getter_output(cg, c(0L, 2L))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "name")
  expect_identical(out$name, c("A", "C"))
})

test_that(".relations builds, resolves, and binds rows when multiple", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "PDAG")
  cg <- cg |> add_edges(D %-->% E) # invalidate build

  # simulate parents_of_ptr by delegating to public parents
  parent_getter <- function(ptr, i0) {
    # translate i0 back to name then use public API to avoid duplicating backend
    nm <- cg@nodes$name[i0 + 1L]
    res <- parents(cg, nm)
    match(res$name, cg@nodes$name) - 1L
  }

  out_single <- caugi:::.relations(cg, "B", NULL, parent_getter)
  expect_identical(out_single$name, "A")

  out_multi <- caugi:::.relations(cg, c("B", "C"), NULL, parent_getter)
  expect_s3_class(out_multi, "tbl_df")
  expect_true(nrow(out_multi) >= 1L)
})

test_that(".relations fails with bad input", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")

  parent_getter <- function(ptr, i0) {
    nm <- cg@nodes$name[i0 + 1L]
    res <- parents(cg, nm)
    match(res$name, cg@nodes$name) - 1L
  }

  expect_error(caugi:::.relations(cg, NULL, NULL, parent_getter), "Supply one of `nodes` or `index`")
  expect_error(caugi:::.relations(cg, "A", 1L, parent_getter), "Supply either `nodes` or `index`")
  expect_error(caugi:::.relations(cg, "Z", NULL, parent_getter), "Unknown node\\(s\\)")
  expect_error(caugi:::.relations(cg, NULL, 0L, parent_getter), "out of bounds")
})
