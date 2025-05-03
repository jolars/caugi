# tests/testthat/test-from-sparse.R

library(testthat)
library(Matrix)
library(caugi)

test_that("as_caugi.dgCMatrix and caugi_from_sparse build correct CSR", {
  mat <- sparseMatrix(
    i = c(1, 1),
    j = c(2, 3),
    dims = c(4, 4),
    dimnames = list(letters[1:4], letters[1:4])
  )

  # direct constructor
  g1 <- caugi_from_sparse(mat, directed = TRUE)
  # S3 generic
  g2 <- as_caugi(mat, directed = TRUE)

  for (g in list(g1, g2)) {
    expect_s3_class(g, "caugi_graph")
    expect_equal(g$nodes$name, letters[1:4])
    # two edges 1→2 and 1→3
    expect_equal(g$csr$row_ptr, c(0L, 2L, 2L, 2L, 2L))
    expect_equal(g$csr$col_ids, c(2L, 3L))
    expect_equal(g$csr$type_codes, c(1L, 1L))
  }

  # as_tibble works too
  df <- as_tibble(g2)
  expect_equal(
    df,
    tibble::tibble(
      from      = c("a", "a"),
      to        = c("b", "c"),
      edge_type = c("-->", "-->")
    )
  )
})
