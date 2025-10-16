# SPDX-License-Identifier: MPL-2.0

cg_from_rc_pd <- function(A) {
  stopifnot(is.matrix(A), nrow(A) == ncol(A))
  n <- nrow(A)
  nm <- colnames(A)
  if (is.null(nm)) nm <- paste0("V", seq_len(n))

  from <- character(0)
  edge <- character(0)
  to <- character(0)

  # directed: row-major, 1 means i -> j
  idx <- which(A == 1L, arr.ind = TRUE)
  if (nrow(idx)) {
    from <- c(from, nm[idx[, 1]])
    edge <- c(edge, rep("-->", nrow(idx)))
    to <- c(to, nm[idx[, 2]])
  }

  # undirected: accept either half, add once
  if (n > 1) {
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        if (A[i, j] == 2L || A[j, i] == 2L) {
          from <- c(from, nm[i])
          edge <- c(edge, "---")
          to <- c(to, nm[j])
        }
      }
    }
  }

  caugi_graph(from = from, edge = edge, to = to, nodes = nm, class = "PDAG")
}

cg_from_rc_dag <- function(A) {
  stopifnot(is.matrix(A), nrow(A) == ncol(A))
  n <- nrow(A)
  nm <- colnames(A)
  if (is.null(nm)) nm <- paste0("V", seq_len(n))
  idx <- which(A != 0L, arr.ind = TRUE)
  from <- nm[idx[, 1]]
  to <- nm[idx[, 2]]
  caugi_graph(from = from, edge = rep("-->", length(from)), to = to, nodes = nm, class = "DAG")
}


test_that("AID wrappers match gadjid_r and known values", {
  # ---- fixtures (copied from gadjid_r tests) ---------------------------------
  cpdag10 <- matrix(0L, 10, 10)
  cpdag10_entries <- cbind(
    row = c(7, 1, 1, 2, 3, 4, 4, 3, 2, 3, 4),
    col = c(2, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10),
    val = c(2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1)
  )
  cpdag10[as.matrix(cpdag10_entries[, 1:2])] <- cpdag10_entries[, 3]

  dag17 <- matrix(0L, 10, 10)
  dag17_entries <- cbind(
    row = c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7),
    col = c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)
  )
  dag17[as.matrix(dag17_entries[, 1:2])] <- 1L

  dag18 <- matrix(0L, 10, 10)
  dag18_entries <- cbind(
    row = c(9, 1, 7, 10, 5, 3, 4, 2, 8),
    col = c(1, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  dag18[as.matrix(dag18_entries[, 1:2])] <- 1L

  cpdag19 <- matrix(0L, 10, 10)
  cpdag19_entries <- cbind(
    row = c(9, 7, 2, 1, 6, 9, 3, 6, 3),
    col = c(1, 2, 4, 5, 5, 7, 8, 8, 10),
    val = c(2, 2, 2, 1, 1, 2, 2, 2, 2)
  )
  cpdag19[as.matrix(cpdag19_entries[, 1:2])] <- cpdag19_entries[, 3]

  g_cpdag10 <- cg_from_rc_pd(cpdag10)
  g_dag17 <- cg_from_rc_dag(dag17)
  g_dag18 <- cg_from_rc_dag(dag18)
  g_cpdag19 <- cg_from_rc_pd(cpdag19)

  # ---- expected reference values (from gadjid_r tests) -----------------------
  # ancestor
  expect_equal(aid(g_cpdag10, g_dag17, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g_cpdag10, g_dag17, type = "ancestor")$count, 23)
  expect_equal(aid(g_dag17, g_dag18, type = "ancestor")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g_dag17, g_dag18, type = "ancestor")$count, 67)

  # oset
  expect_equal(aid(g_cpdag10, g_dag17, type = "oset")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g_cpdag10, g_dag17, type = "oset")$count, 23)
  expect_equal(aid(g_dag17, g_dag18, type = "oset")$score, 63 / 90, tolerance = 1e-12)
  expect_equal(aid(g_dag17, g_dag18, type = "oset")$count, 63)

  # parent
  expect_equal(aid(g_cpdag10, g_dag17, type = "parent")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g_cpdag10, g_dag17, type = "parent")$count, 23)
  expect_equal(aid(g_dag17, g_dag18, type = "parent")$score, 85 / 90, tolerance = 1e-12)
  expect_equal(aid(g_dag17, g_dag18, type = "parent")$count, 85)
})
