# tests/testthat/test-roundtrip.R
test_that("CPDAG: amat → CSR → amat is loss-free", {
  ## 4-node example
  ##   1 → 2,  2 — 3,  3 → 4
  amat <- matrix(0L, 4, 4)

  # 1 → 2  (tail at 1, arrowhead at 2)
  amat[1, 2] <- 0L
  amat[2, 1] <- 1L

  # 2 — 3  (undirected)
  amat[2, 3] <- 1L
  amat[3, 2] <- 1L

  # 3 → 4
  amat[3, 4] <- 0L
  amat[4, 3] <- 1L

  csr <- caugi_create_csr_from_amat(amat, "cpdag")
  amat2 <- caugi_create_amat_from_csr(
    csr$row_ptr, csr$col_ids, csr$type_codes, "cpdag"
  )

  expect_equal(amat2, amat, ignore_attr = TRUE)
})

test_that("PAG: amat → CSR → amat is loss-free", {
  ## 3-node example with a variety of marks
  ##   1 → 2,  2 o→ 3,  3 — 1 (tails both ends)
  amat <- matrix(0L, 3, 3)

  # 1 → 2  (arrowhead at 2, tail at 1)
  amat[1, 2] <- 2L # arrowhead
  amat[2, 1] <- 3L # tail

  # 2 o→ 3  (circle at 3, arrowhead at 2)
  amat[2, 3] <- 1L # circle
  amat[3, 2] <- 2L # arrowhead

  # 3 — 1   (tail–tail / undirected in PAG)
  amat[3, 1] <- 3L
  amat[1, 3] <- 3L

  csr <- caugi_create_csr_from_amat(amat, "pag")
  amat2 <- caugi_create_amat_from_csr(
    csr$row_ptr, csr$col_ids, csr$type_codes, "pag"
  )

  expect_equal(amat2, amat, ignore_attr = TRUE)
})
