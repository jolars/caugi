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
  # by index
  expect_identical(parents(cg, 2)$name, c("A", "D"))
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
  expect_identical(children(cg, 1)$name, "B")
})

test_that("neighbors returns undirected and directed adjacency", {
  cg <- caugi_graph(A %-->% B, B %-->% C, B %---% D, C %---% E, class = "PDAG")
  # B has neighbors A (incoming), C (outgoing), D (undirected)
  expect_identical(sort(neighbors(cg, "B")$name), c("A", "C", "D"))
  # C has neighbors B and E
  expect_identical(sort(neighbors(cg, "C")$name), c("B", "E"))
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
  expect_error(children(cg, 0), ">= 1")
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

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Getter helpers ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that(".resolve_idx maps names and 1-based indices to 0-based", {
  cg <- caugi_graph(A %-->% B, B %-->% C, class = "DAG")
  # names
  expect_identical(caugi:::.resolve_idx(cg, c("A", "C")), c(0L, 2L))
  # numeric 1-based
  expect_identical(caugi:::.resolve_idx(cg, c(1, 3)), c(0L, 2L))
  # mixed numeric as integer
  expect_identical(caugi:::.resolve_idx(cg, 2L), 1L)
  # bad index
  expect_error(caugi:::.resolve_idx(cg, 0), ">= 1")
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

test_that(".capture_nodes_expr accepts unquoted, vectors, and expressions", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %---% D, class = "PDAG")
  env <- environment()
  A <- "SHADOW" # ensure unquoted is not captured from env accidentally

  # unquoted symbol
  expr1 <- substitute(B)
  got1 <- caugi:::.capture_nodes_expr(expr1, env)
  expect_identical(got1, "B")

  # plus expression
  expr2 <- substitute(A + C)
  got2 <- caugi:::.capture_nodes_expr(expr2, env)
  expect_identical(sort(got2), c("A", "C"))

  # c() expression
  expr3 <- substitute(c(A, D))
  got3 <- caugi:::.capture_nodes_expr(expr3, env)
  expect_identical(sort(got3), c("A", "D"))

  # direct value passthrough
  got4 <- caugi:::.capture_nodes_expr(quote(letters[1:2]), env)
  expect_identical(got4, c("a", "b"))
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

  out_single <- caugi:::.relations(cg, "B", parent_getter)
  expect_identical(out_single$name, "A")

  out_multi <- caugi:::.relations(cg, c("B", "C"), parent_getter)
  expect_s3_class(out_multi, "tbl_df")
  expect_true(nrow(out_multi) >= 1L)
})
