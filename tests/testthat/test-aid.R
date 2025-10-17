# SPDX-License-Identifier: MPL-2.0

# Copies generated from gadjid tests.

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

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-CPDAG-16.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 14)
  expect_equal(aid(g1, g2, type = "oset")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 14)
  expect_equal(aid(g1, g2, type = "parent")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 26)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 23)
  expect_equal(aid(g1, g2, type = "parent")$score, 31 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 31)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 18 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 18)
  expect_equal(aid(g1, g2, type = "oset")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 21)
  expect_equal(aid(g1, g2, type = "parent")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 33)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 17 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 17)
  expect_equal(aid(g1, g2, type = "oset")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 21)
  expect_equal(aid(g1, g2, type = "parent")$score, 34 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 34)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 13)
  expect_equal(aid(g1, g2, type = "oset")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 13)
  expect_equal(aid(g1, g2, type = "parent")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 24)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 14)
  expect_equal(aid(g1, g2, type = "oset")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 14)
  expect_equal(aid(g1, g2, type = "parent")$score, 31 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 31)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 14)
  expect_equal(aid(g1, g2, type = "oset")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 14)
  expect_equal(aid(g1, g2, type = "parent")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 26)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 16 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 16)
  expect_equal(aid(g1, g2, type = "oset")$score, 16 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 16)
  expect_equal(aid(g1, g2, type = "parent")$score, 16 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 16)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 8 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 8)
  expect_equal(aid(g1, g2, type = "parent")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 20)
})

test_that("small_dag: 10-node-DAG-10.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 18 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 18)
  expect_equal(aid(g1, g2, type = "oset")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 19)
  expect_equal(aid(g1, g2, type = "parent")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 26)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 29 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 29)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 35 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 35)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 25)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 38)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 22 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 22)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 33)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 23)
  expect_equal(aid(g1, g2, type = "parent")$score, 35 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 35)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 34 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 34)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 29 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 29)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 30 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 30)
  expect_equal(aid(g1, g2, type = "oset")$score, 30 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 30)
  expect_equal(aid(g1, g2, type = "parent")$score, 30 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 30)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 39)
  expect_equal(aid(g1, g2, type = "oset")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 26)
  expect_equal(aid(g1, g2, type = "parent")$score, 42 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 42)
})

test_that("small_dag: 10-node-DAG-11.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 19)
  expect_equal(aid(g1, g2, type = "oset")$score, 22 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 22)
  expect_equal(aid(g1, g2, type = "parent")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 40)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 13)
  expect_equal(aid(g1, g2, type = "oset")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 13)
  expect_equal(aid(g1, g2, type = "parent")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 21)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 16 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 16)
  expect_equal(aid(g1, g2, type = "oset")$score, 17 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 17)
  expect_equal(aid(g1, g2, type = "parent")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 21)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 15 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 15)
  expect_equal(aid(g1, g2, type = "oset")$score, 16 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 16)
  expect_equal(aid(g1, g2, type = "parent")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 24)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 15 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 15)
  expect_equal(aid(g1, g2, type = "oset")$score, 15 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 15)
  expect_equal(aid(g1, g2, type = "parent")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 24)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 13)
  expect_equal(aid(g1, g2, type = "oset")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 14)
  expect_equal(aid(g1, g2, type = "parent")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 26)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 14)
  expect_equal(aid(g1, g2, type = "oset")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 14)
  expect_equal(aid(g1, g2, type = "parent")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 24)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 13)
  expect_equal(aid(g1, g2, type = "oset")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 13)
  expect_equal(aid(g1, g2, type = "parent")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 21)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 14)
  expect_equal(aid(g1, g2, type = "oset")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 14)
  expect_equal(aid(g1, g2, type = "parent")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 14)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 36 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 36)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 27 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 27)
})

test_that("small_dag: 10-node-DAG-12.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 17 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 17)
  expect_equal(aid(g1, g2, type = "oset")$score, 14 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 14)
  expect_equal(aid(g1, g2, type = "parent")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 19)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 44 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 44)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 24)
  expect_equal(aid(g1, g2, type = "oset")$score, 27 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 27)
  expect_equal(aid(g1, g2, type = "parent")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 46)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 26)
  expect_equal(aid(g1, g2, type = "parent")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 39)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 25)
  expect_equal(aid(g1, g2, type = "oset")$score, 27 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 27)
  expect_equal(aid(g1, g2, type = "parent")$score, 53 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 53)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 29 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 29)
  expect_equal(aid(g1, g2, type = "oset")$score, 34 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 34)
  expect_equal(aid(g1, g2, type = "parent")$score, 53 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 53)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 17 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 17)
  expect_equal(aid(g1, g2, type = "oset")$score, 17 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 17)
  expect_equal(aid(g1, g2, type = "parent")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 40)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 44 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 44)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 39)
  expect_equal(aid(g1, g2, type = "oset")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 39)
  expect_equal(aid(g1, g2, type = "parent")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 39)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 24)
  expect_equal(aid(g1, g2, type = "parent")$score, 53 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 53)
})

test_that("small_dag: 10-node-DAG-13.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 25)
  expect_equal(aid(g1, g2, type = "oset")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 24)
  expect_equal(aid(g1, g2, type = "parent")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 45)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 19)
  expect_equal(aid(g1, g2, type = "oset")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 19)
  expect_equal(aid(g1, g2, type = "parent")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 49)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 19)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 38)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 21)
  expect_equal(aid(g1, g2, type = "oset")$score, 27 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 27)
  expect_equal(aid(g1, g2, type = "parent")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 40)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 27 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 27)
  expect_equal(aid(g1, g2, type = "oset")$score, 27 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 27)
  expect_equal(aid(g1, g2, type = "parent")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 49)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 30 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 30)
  expect_equal(aid(g1, g2, type = "oset")$score, 32 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 32)
  expect_equal(aid(g1, g2, type = "parent")$score, 54 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 54)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 23)
  expect_equal(aid(g1, g2, type = "parent")$score, 60 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 60)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 19)
  expect_equal(aid(g1, g2, type = "oset")$score, 19 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 19)
  expect_equal(aid(g1, g2, type = "parent")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 49)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 25)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 25)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 33)
  expect_equal(aid(g1, g2, type = "oset")$score, 30 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 30)
  expect_equal(aid(g1, g2, type = "parent")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 49)
})

test_that("small_dag: 10-node-DAG-14.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 33)
  expect_equal(aid(g1, g2, type = "oset")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 33)
  expect_equal(aid(g1, g2, type = "parent")$score, 54 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 54)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 6)
  expect_equal(aid(g1, g2, type = "oset")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 6)
  expect_equal(aid(g1, g2, type = "parent")$score, 9 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 9)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 8 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 8)
  expect_equal(aid(g1, g2, type = "oset")$score, 8 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 8)
  expect_equal(aid(g1, g2, type = "parent")$score, 11 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 11)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 6)
  expect_equal(aid(g1, g2, type = "oset")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 6)
  expect_equal(aid(g1, g2, type = "parent")$score, 9 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 9)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 6)
  expect_equal(aid(g1, g2, type = "oset")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 6)
  expect_equal(aid(g1, g2, type = "parent")$score, 9 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 9)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 4 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 4)
  expect_equal(aid(g1, g2, type = "oset")$score, 4 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 4)
  expect_equal(aid(g1, g2, type = "parent")$score, 9 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 9)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 10 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 10)
  expect_equal(aid(g1, g2, type = "oset")$score, 10 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 10)
  expect_equal(aid(g1, g2, type = "parent")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 13)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 6)
  expect_equal(aid(g1, g2, type = "oset")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 6)
  expect_equal(aid(g1, g2, type = "parent")$score, 9 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 9)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 13)
  expect_equal(aid(g1, g2, type = "oset")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 13)
  expect_equal(aid(g1, g2, type = "parent")$score, 13 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 13)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 6)
  expect_equal(aid(g1, g2, type = "oset")$score, 6 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 6)
  expect_equal(aid(g1, g2, type = "parent")$score, 8 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 8)
})

test_that("small_dag: 10-node-DAG-15.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 8 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 8)
  expect_equal(aid(g1, g2, type = "oset")$score, 8 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 8)
  expect_equal(aid(g1, g2, type = "parent")$score, 8 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 8)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-16.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 45)
  expect_equal(aid(g1, g2, type = "parent")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 81)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 51)
  expect_equal(aid(g1, g2, type = "parent")$score, 83 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 83)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 52)
  expect_equal(aid(g1, g2, type = "oset")$score, 54 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 54)
  expect_equal(aid(g1, g2, type = "parent")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 75)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 48)
  expect_equal(aid(g1, g2, type = "oset")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 49)
  expect_equal(aid(g1, g2, type = "parent")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 73)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 54 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 54)
  expect_equal(aid(g1, g2, type = "oset")$score, 54 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 54)
  expect_equal(aid(g1, g2, type = "parent")$score, 83 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 83)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 52)
  expect_equal(aid(g1, g2, type = "parent")$score, 74 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 74)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 49)
  expect_equal(aid(g1, g2, type = "oset")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 49)
  expect_equal(aid(g1, g2, type = "parent")$score, 79 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 79)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 45)
  expect_equal(aid(g1, g2, type = "parent")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 81)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 67)
  expect_equal(aid(g1, g2, type = "oset")$score, 63 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 63)
  expect_equal(aid(g1, g2, type = "parent")$score, 85 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 85)
})

test_that("small_dag: 10-node-DAG-17.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 47)
  expect_equal(aid(g1, g2, type = "oset")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 48)
  expect_equal(aid(g1, g2, type = "parent")$score, 76 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 76)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 45)
  expect_equal(aid(g1, g2, type = "parent")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 45)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 31 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 31)
  expect_equal(aid(g1, g2, type = "oset")$score, 36 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 36)
  expect_equal(aid(g1, g2, type = "parent")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 20)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 47)
  expect_equal(aid(g1, g2, type = "oset")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 48)
  expect_equal(aid(g1, g2, type = "parent")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 47)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 49)
  expect_equal(aid(g1, g2, type = "oset")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 48)
  expect_equal(aid(g1, g2, type = "parent")$score, 60 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 60)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 38)
  expect_equal(aid(g1, g2, type = "oset")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 46)
  expect_equal(aid(g1, g2, type = "parent")$score, 44 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 44)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 46)
  expect_equal(aid(g1, g2, type = "oset")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 48)
  expect_equal(aid(g1, g2, type = "parent")$score, 62 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 62)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 44 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 44)
  expect_equal(aid(g1, g2, type = "oset")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 45)
  expect_equal(aid(g1, g2, type = "parent")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 46)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 45)
  expect_equal(aid(g1, g2, type = "parent")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 45)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 52)
  expect_equal(aid(g1, g2, type = "oset")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 52)
  expect_equal(aid(g1, g2, type = "parent")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 52)
})

test_that("small_dag: 10-node-DAG-18.mtx vs 10-node-DAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 43 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 43)
  expect_equal(aid(g1, g2, type = "parent")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 52)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-CPDAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 25)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 25)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(7, 1, 1, 2, 3, 4, 2, 3, 2, 3, 4), c(4, 5, 6, 6, 6, 6, 7, 8, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 24)
  expect_equal(aid(g1, g2, type = "parent")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 23)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 4, 10, 6, 9, 5, 7, 10, 3), c(1, 1, 1, 1, 5, 6, 7, 7, 8, 8, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 22 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 22)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 26)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 3, 4, 8, 10, 6, 8, 10, 8, 9), c(1, 2, 2, 2, 2, 5, 5, 5, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 24)
  expect_equal(aid(g1, g2, type = "parent")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 20)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 10, 7, 9, 10, 3, 6, 7, 9, 1, 6, 7, 10), c(2, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 8, 8, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 33)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 1, 3, 1, 4, 5, 10, 8, 10, 1, 5, 3, 1), c(1, 2, 2, 6, 6, 6, 6, 7, 7, 8, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 29 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 29)
  expect_equal(aid(g1, g2, type = "oset")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 33)
  expect_equal(aid(g1, g2, type = "parent")$score, 31 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 31)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(10, 6, 9, 1, 10, 9), c(1, 3, 3, 5, 5, 8)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 24)
  expect_equal(aid(g1, g2, type = "oset")$score, 24 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 24)
  expect_equal(aid(g1, g2, type = "parent")$score, 22 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 22)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-16.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 25)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 25)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(4, 1, 3, 4, 6, 7, 8, 10, 1, 4, 1, 2, 3, 4, 6, 7, 8, 9, 10, 1, 3, 4, 1, 3, 4, 6, 1, 3, 4, 6, 7, 10, 1, 2, 3, 4, 6, 7, 8, 10, 1, 3, 4, 6, 7), c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 21)
  expect_equal(aid(g1, g2, type = "oset")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 21)
  expect_equal(aid(g1, g2, type = "parent")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 21)
})

test_that("small_dag: 10-node-DAG-19.mtx vs 10-node-DAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 4, 10, 1, 6, 8, 2, 3, 7), c(1, 2, 3, 5, 5, 6, 7, 8, 9)))] <- 1L
    cg_from_rc_dag(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(9, 1, 7, 10, 5, 3, 4, 2, 8), c(1, 3, 4, 5, 6, 7, 8, 9, 10)))] <- 1L
    cg_from_rc_dag(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 17 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 17)
  expect_equal(aid(g1, g2, type = "oset")$score, 18 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 18)
  expect_equal(aid(g1, g2, type = "parent")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 33)
})


test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 38)
  expect_equal(aid(g1, g2, type = "oset")$score, 37 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 37)
  expect_equal(aid(g1, g2, type = "parent")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 46)
})

test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 36 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 36)
  expect_equal(aid(g1, g2, type = "oset")$score, 37 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 37)
  expect_equal(aid(g1, g2, type = "parent")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 48)
})

test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 37 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 37)
  expect_equal(aid(g1, g2, type = "oset")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 39)
  expect_equal(aid(g1, g2, type = "parent")$score, 53 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 53)
})

test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 46)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})

test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 26)
  expect_equal(aid(g1, g2, type = "oset")$score, 26 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 26)
  expect_equal(aid(g1, g2, type = "parent")$score, 43 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 43)
})

test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 71)
  expect_equal(aid(g1, g2, type = "oset")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 71)
  expect_equal(aid(g1, g2, type = "parent")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 71)
})

test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 71)
  expect_equal(aid(g1, g2, type = "oset")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 71)
  expect_equal(aid(g1, g2, type = "parent")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 71)
})

test_that("small_cpdag: 10-node-CPDAG-10.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 40)
  expect_equal(aid(g1, g2, type = "oset")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 39)
  expect_equal(aid(g1, g2, type = "parent")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 46)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 40)
  expect_equal(aid(g1, g2, type = "oset")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 41)
  expect_equal(aid(g1, g2, type = "parent")$score, 50 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 50)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 34 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 34)
  expect_equal(aid(g1, g2, type = "oset")$score, 35 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 35)
  expect_equal(aid(g1, g2, type = "parent")$score, 50 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 50)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 46)
  expect_equal(aid(g1, g2, type = "parent")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 56)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 37 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 37)
  expect_equal(aid(g1, g2, type = "oset")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 40)
  expect_equal(aid(g1, g2, type = "parent")$score, 50 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 50)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 28 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 28)
  expect_equal(aid(g1, g2, type = "oset")$score, 28 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 28)
  expect_equal(aid(g1, g2, type = "parent")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 40)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 73)
  expect_equal(aid(g1, g2, type = "oset")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 73)
  expect_equal(aid(g1, g2, type = "parent")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 73)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 73)
  expect_equal(aid(g1, g2, type = "oset")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 73)
  expect_equal(aid(g1, g2, type = "parent")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 73)
})

test_that("small_cpdag: 10-node-CPDAG-11.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 47)
  expect_equal(aid(g1, g2, type = "oset")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 48)
  expect_equal(aid(g1, g2, type = "parent")$score, 60 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 60)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 34 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 34)
  expect_equal(aid(g1, g2, type = "oset")$score, 35 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 35)
  expect_equal(aid(g1, g2, type = "parent")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 41)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 32 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 32)
  expect_equal(aid(g1, g2, type = "oset")$score, 32 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 32)
  expect_equal(aid(g1, g2, type = "parent")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 41)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 34 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 34)
  expect_equal(aid(g1, g2, type = "oset")$score, 34 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 34)
  expect_equal(aid(g1, g2, type = "parent")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 39)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 35 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 35)
  expect_equal(aid(g1, g2, type = "oset")$score, 35 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 35)
  expect_equal(aid(g1, g2, type = "parent")$score, 43 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 43)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 21)
  expect_equal(aid(g1, g2, type = "oset")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 21)
  expect_equal(aid(g1, g2, type = "parent")$score, 29 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 29)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 75)
  expect_equal(aid(g1, g2, type = "oset")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 75)
  expect_equal(aid(g1, g2, type = "parent")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 75)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 75)
  expect_equal(aid(g1, g2, type = "oset")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 75)
  expect_equal(aid(g1, g2, type = "parent")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 75)
})

test_that("small_cpdag: 10-node-CPDAG-12.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 41)
  expect_equal(aid(g1, g2, type = "oset")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 41)
  expect_equal(aid(g1, g2, type = "parent")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 47)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 39)
  expect_equal(aid(g1, g2, type = "oset")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 41)
  expect_equal(aid(g1, g2, type = "parent")$score, 59 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 59)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 47)
  expect_equal(aid(g1, g2, type = "oset")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 47)
  expect_equal(aid(g1, g2, type = "parent")$score, 58 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 58)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 38)
  expect_equal(aid(g1, g2, type = "oset")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 40)
  expect_equal(aid(g1, g2, type = "parent")$score, 55 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 55)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 52)
  expect_equal(aid(g1, g2, type = "parent")$score, 70 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 70)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 37 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 37)
  expect_equal(aid(g1, g2, type = "oset")$score, 37 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 37)
  expect_equal(aid(g1, g2, type = "parent")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 56)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 67)
  expect_equal(aid(g1, g2, type = "oset")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 67)
  expect_equal(aid(g1, g2, type = "parent")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 67)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 67)
  expect_equal(aid(g1, g2, type = "oset")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 67)
  expect_equal(aid(g1, g2, type = "parent")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 67)
})

test_that("small_cpdag: 10-node-CPDAG-13.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 39)
  expect_equal(aid(g1, g2, type = "oset")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 39)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 49)
  expect_equal(aid(g1, g2, type = "oset")$score, 49 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 49)
  expect_equal(aid(g1, g2, type = "parent")$score, 59 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 59)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 39)
  expect_equal(aid(g1, g2, type = "oset")$score, 40 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 40)
  expect_equal(aid(g1, g2, type = "parent")$score, 52 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 52)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 38)
  expect_equal(aid(g1, g2, type = "oset")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 38)
  expect_equal(aid(g1, g2, type = "parent")$score, 61 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 61)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 47 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 47)
  expect_equal(aid(g1, g2, type = "oset")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 51)
  expect_equal(aid(g1, g2, type = "parent")$score, 58 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 58)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 44 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 44)
  expect_equal(aid(g1, g2, type = "oset")$score, 44 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 44)
  expect_equal(aid(g1, g2, type = "parent")$score, 62 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 62)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 56)
  expect_equal(aid(g1, g2, type = "oset")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 56)
  expect_equal(aid(g1, g2, type = "parent")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 56)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 56)
  expect_equal(aid(g1, g2, type = "oset")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 56)
  expect_equal(aid(g1, g2, type = "parent")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 56)
})

test_that("small_cpdag: 10-node-CPDAG-14.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 50 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 50)
  expect_equal(aid(g1, g2, type = "oset")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 51)
  expect_equal(aid(g1, g2, type = "parent")$score, 61 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 61)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 25)
  expect_equal(aid(g1, g2, type = "oset")$score, 25 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 25)
  expect_equal(aid(g1, g2, type = "parent")$score, 27 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 27)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 23)
  expect_equal(aid(g1, g2, type = "oset")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 23)
  expect_equal(aid(g1, g2, type = "parent")$score, 23 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 23)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 20)
  expect_equal(aid(g1, g2, type = "oset")$score, 20 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 20)
  expect_equal(aid(g1, g2, type = "parent")$score, 21 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 21)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 32 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 32)
  expect_equal(aid(g1, g2, type = "oset")$score, 32 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 32)
  expect_equal(aid(g1, g2, type = "parent")$score, 33 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 33)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 38)
  expect_equal(aid(g1, g2, type = "oset")$score, 38 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 38)
  expect_equal(aid(g1, g2, type = "parent")$score, 37 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 37)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 81)
  expect_equal(aid(g1, g2, type = "oset")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 81)
  expect_equal(aid(g1, g2, type = "parent")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 81)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 81)
  expect_equal(aid(g1, g2, type = "oset")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 81)
  expect_equal(aid(g1, g2, type = "parent")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 81)
})

test_that("small_cpdag: 10-node-CPDAG-15.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 45)
  expect_equal(aid(g1, g2, type = "parent")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 45)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 71)
  expect_equal(aid(g1, g2, type = "oset")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 71)
  expect_equal(aid(g1, g2, type = "parent")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 71)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 73)
  expect_equal(aid(g1, g2, type = "oset")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 73)
  expect_equal(aid(g1, g2, type = "parent")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 73)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 75)
  expect_equal(aid(g1, g2, type = "oset")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 75)
  expect_equal(aid(g1, g2, type = "parent")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 75)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 67)
  expect_equal(aid(g1, g2, type = "oset")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 67)
  expect_equal(aid(g1, g2, type = "parent")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 67)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 56)
  expect_equal(aid(g1, g2, type = "oset")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 56)
  expect_equal(aid(g1, g2, type = "parent")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 56)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 81)
  expect_equal(aid(g1, g2, type = "oset")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 81)
  expect_equal(aid(g1, g2, type = "parent")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 81)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_cpdag: 10-node-CPDAG-17.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 51)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 71)
  expect_equal(aid(g1, g2, type = "oset")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 71)
  expect_equal(aid(g1, g2, type = "parent")$score, 71 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 71)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 73)
  expect_equal(aid(g1, g2, type = "oset")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 73)
  expect_equal(aid(g1, g2, type = "parent")$score, 73 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 73)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 75)
  expect_equal(aid(g1, g2, type = "oset")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 75)
  expect_equal(aid(g1, g2, type = "parent")$score, 75 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 75)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 67)
  expect_equal(aid(g1, g2, type = "oset")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 67)
  expect_equal(aid(g1, g2, type = "parent")$score, 67 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 67)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 56)
  expect_equal(aid(g1, g2, type = "oset")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 56)
  expect_equal(aid(g1, g2, type = "parent")$score, 56 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 56)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 81)
  expect_equal(aid(g1, g2, type = "oset")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 81)
  expect_equal(aid(g1, g2, type = "parent")$score, 81 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 81)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 0)
  expect_equal(aid(g1, g2, type = "oset")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 0)
  expect_equal(aid(g1, g2, type = "parent")$score, 0 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 0)
})

test_that("small_cpdag: 10-node-CPDAG-18.mtx vs 10-node-CPDAG-19.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 51)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-10.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 2, 3, 4, 2, 3, 4), c(6, 6, 6, 6, 10, 10, 10)))] <- 1L
    A[as.matrix(cbind(c(7, 1, 4, 3), c(2, 5, 7, 8)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 39)
  expect_equal(aid(g1, g2, type = "oset")$score, 39 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 39)
  expect_equal(aid(g1, g2, type = "parent")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 45)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-11.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 7, 10, 6, 9, 5, 7, 10), c(1, 1, 1, 1, 7, 7, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 4, 6), c(3, 5, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 46 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 46)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-12.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(3, 4, 8, 10, 6, 8, 10), c(2, 2, 2, 2, 5, 5, 5)))] <- 1L
    A[as.matrix(cbind(c(3, 9, 9), c(1, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 41)
  expect_equal(aid(g1, g2, type = "oset")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 41)
  expect_equal(aid(g1, g2, type = "parent")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 45)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-13.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 10, 7, 9, 10, 3, 6, 1, 6, 7, 10), c(3, 3, 4, 4, 4, 5, 5, 8, 8, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(9, 6, 9), c(2, 7, 7)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 36 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 36)
  expect_equal(aid(g1, g2, type = "oset")$score, 36 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 36)
  expect_equal(aid(g1, g2, type = "parent")$score, 41 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 41)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-14.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 4, 5, 10, 8, 10, 1, 5), c(6, 6, 6, 6, 7, 7, 8, 8)))] <- 1L
    A[as.matrix(cbind(c(1, 1, 2, 3, 1), c(2, 3, 3, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 48)
  expect_equal(aid(g1, g2, type = "oset")$score, 48 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 48)
  expect_equal(aid(g1, g2, type = "parent")$score, 53 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 53)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-15.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(6, 9), c(3, 3)))] <- 1L
    A[as.matrix(cbind(c(1, 10, 8, 1), c(5, 5, 9, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 45)
  expect_equal(aid(g1, g2, type = "oset")$score, 45 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 45)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-17.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(2, 3, 3, 6, 7, 8, 9, 10, 4, 5, 7, 8, 10, 1, 2, 5, 1, 2, 6, 9, 1, 3, 4, 7, 8, 9, 10, 1, 4, 5, 8, 9, 10, 1, 4, 5, 9, 10, 1, 3, 4, 10, 1, 4, 5), c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 51)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})

test_that("small_cpdag: 10-node-CPDAG-19.mtx vs 10-node-CPDAG-18.mtx", {
  g1 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 6), c(5, 5)))] <- 1L
    A[as.matrix(cbind(c(9, 7, 2, 9, 3, 6, 3), c(1, 2, 4, 7, 8, 8, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  g2 <- local({
    A <- matrix(0L, 10, 10)
    A[as.matrix(cbind(c(1, 7, 6, 4, 4, 1, 2, 5, 8), c(3, 3, 5, 7, 8, 9, 9, 10, 10)))] <- 2L
    cg_from_rc_pd(A)
  })
  expect_equal(aid(g1, g2, type = "ancestor")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "ancestor")$count, 51)
  expect_equal(aid(g1, g2, type = "oset")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "oset")$count, 51)
  expect_equal(aid(g1, g2, type = "parent")$score, 51 / 90, tolerance = 1e-12)
  expect_equal(aid(g1, g2, type = "parent")$count, 51)
})
