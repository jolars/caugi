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
# ────────────────────────── Convert between types ─────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────
