# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Security tests ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# These tests check that dangerous manipulations of the graph state are caught
# and handled properly by the package.
# ──────────────────────────────────────────────────────────────────────────────

test_that("build() errors when using dangerous manipulation", {
  cg <- caugi_graph(A %-->% B, B %-->% C, C %-->% D, class = "DAG")

  # dangerous manipulation only done to test the function
  # introduce cycle
  try(cg@.state$edges$to[3] <- "A", silent = TRUE)
  expect_error(cg <- build(cg), "cycle")
})

test_that("fingerprint updates when building", {
  cg <- caugi_graph(A %-->% B)
  current_fingerprint <- cg@fingerprint

  cg <- add_edges(cg, A %-->% C)
  expect_true(identical(cg@fingerprint, current_fingerprint)) # not built yet

  cg <- build(cg)
  # fingerprint updated by build
  expect_false(identical(cg@fingerprint, current_fingerprint))
})
