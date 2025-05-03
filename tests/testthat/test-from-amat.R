# tests/testthat/test-from-amat.R

library(testthat)
library(caugi)
library(tibble)

test_that("caugi_from_amat errors on non-amat", {
  m <- matrix(0L, 2, 2)
  expect_error(
    caugi_from_amat(m)
  )
})

test_that("caugi_from_amat handles a simple CPDAG amat", {
  # a simple chain A→B→C encoded as CPDAG (0/1 coding)
  mat_cpdag <- matrix(
    c(
      0L, 1L, 0L,
      0L, 0L, 1L,
      0L, 0L, 0L
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(letters[1:3], letters[1:3])
  )
  class(mat_cpdag) <- c("amat", class(mat_cpdag))
  attr(mat_cpdag, "type") <- "cpdag"
  g <- caugi_from_amat(mat_cpdag)
  df <- as_tibble(g)

  expect_equal(
    df,
    tibble::tibble(
      from      = c("b", "c"),
      to        = c("a", "b"),
      edge_type = c("-->", "-->")
    )
  )
})

test_that("caugi_from_amat handles an empty amat", {
  # a simple chain A→B→C encoded as CPDAG (0/1 coding)
  mat_cpdag <- matrix(
    c(
      0L, 0L, 0L,
      0L, 0L, 0L,
      0L, 0L, 0L
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(letters[1:3], letters[1:3])
  )
  class(mat_cpdag) <- c("amat", class(mat_cpdag))
  attr(mat_cpdag, "type") <- "cpdag"
  g <- caugi_from_amat(mat_cpdag)
  df1 <- as_tibble(g)

  attr(mat_cpdag, "type") <- "pag"
  g <- caugi_from_amat(mat_cpdag)
  df2 <- as_tibble(g)

  for (df in list(df1, df2)) {
    expect_equal(
      df,
      tibble::tibble(
        from      = character(),
        to        = character(),
        edge_type = character()
      )
    )
  }
})

test_that("caugi_from_amat handles a simple PAG amat", {
  mat_pag <- matrix(0L, 5, 5,
    dimnames = list(LETTERS[1:5], LETTERS[1:5])
  )
  # A-->B: mat[1,2]=2 arrow, mat[2,1]=3 tail
  mat_pag[1, 2] <- 2L
  mat_pag[2, 1] <- 3L
  # A o-o C: mat[1,3]=1 circle, mat[3,1]=1 circle
  mat_pag[1, 3] <- 1L
  mat_pag[3, 1] <- 1L
  # B<->C: mat[2,3]=2 arrow, mat[3,2]=2 arrow
  mat_pag[2, 3] <- 2L
  mat_pag[3, 2] <- 2L
  # C o-- D: mat[3,4]=1 circle, mat[4,3]=3 tail
  mat_pag[3, 4] <- 1L
  mat_pag[4, 3] <- 3L
  # D o-> E: mat[4,5]=1 circle, mat[5,4]=2 arrow
  mat_pag[4, 5] <- 1L
  mat_pag[5, 4] <- 2L

  class(mat_pag) <- c("amat", class(mat_pag))
  attr(mat_pag, "type") <- "pag"

  g <- caugi_from_amat(mat_pag)
  df <- as_tibble(g)

  expect_equal(
    df,
    tibble::tibble(
      from      = c("A", "A", "B", "C", "D"),
      to        = c("B", "C", "C", "D", "E"),
      edge_type = c("-->", "o-o", "<->", "o--", "o->")
    )
  )
})

test_that("caugi_from_amat handles a simple PAG amat with 0 endpoint", {
  mat_pag <- matrix(0L, 3, 3,
    dimnames = list(LETTERS[1:3], LETTERS[1:3])
  )
  # A-->B: mat[1,2]=2 arrow, mat[2,1]=3 tail
  mat_pag[1, 2] <- 3L
  mat_pag[2, 1] <- 3L
  # A o-o C: mat[1,3]=1 circle, mat[3,1]=1 circle
  mat_pag[1, 3] <- 1L
  mat_pag[3, 1] <- 3L
  # B<->C: mat[2,3]=2 arrow, mat[3,2]=0 -> no edge :(
  mat_pag[2, 3] <- 2L
  mat_pag[3, 2] <- 0L

  class(mat_pag) <- c("amat", class(mat_pag))
  attr(mat_pag, "type") <- "pag"

  g <- caugi_from_amat(mat_pag)
  df <- as_tibble(g)

  expect_equal(
    df,
    tibble::tibble(
      from      = c("A", "A"),
      to        = c("B", "C"),
      edge_type = c("---", "o--")
    )
  )
})

test_that("caugi_from_amat throws error for non integer matrix", {
  mat_pag <- matrix(0L, 3, 3,
    dimnames = list(LETTERS[1:3], LETTERS[1:3])
  )
  # A-->B: mat[1,2]=2 arrow, mat[2,1]=3 tail
  mat_pag[1, 2] <- 3.4
  mat_pag[2, 1] <- 3L
  # A o-o C: mat[1,3]=1 circle, mat[3,1]=1 circle
  mat_pag[1, 3] <- 1L
  mat_pag[3, 1] <- 3L
  # B<->C: mat[2,3]=2 arrow, mat[3,2]=0 -> no edge :(
  mat_pag[2, 3] <- 2L
  mat_pag[3, 2] <- 0L

  class(mat_pag) <- c("amat", class(mat_pag))
  attr(mat_pag, "type") <- "pag"

  expect_error(
    caugi_from_amat(mat_pag)
  )
})
