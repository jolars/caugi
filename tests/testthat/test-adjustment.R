# example seen in Elements of Causal Inference, Figure 6.5, p. 115
adjustment_set_cg <- caugi_graph(
  C %-->% X,
  X %-->% F,
  X %-->% D,
  A %-->% X,
  A %-->% K,
  K %-->% Y,
  D %-->% Y,
  D %-->% G,
  Y %-->% H,
  class = "DAG"
)

names_of <- function(tbl) {
  tbl |>
    dplyr::pull(name) |>
    sort()
}

test_that("d-separation checks", {
  expect_false(d_separated(adjustment_set_cg, X, Y)) # backdoor via A-->K-->Y and causal X-->D-->Y
  expect_false(d_separated(adjustment_set_cg, X, Y, Z = A)) # causal path still open
  expect_false(d_separated(adjustment_set_cg, X, Y, Z = K)) # causal path still open
  expect_false(d_separated(adjustment_set_cg, X, Y, Z = D)) # backdoor still open
  expect_true(d_separated(adjustment_set_cg, X, Y, Z = c(A, D))) # backdoor and causal both blocked
  expect_true(d_separated(adjustment_set_cg, X, Y, Z = c(K, D))) # backdoor and causal both blocked
})

test_that("adjustment_set(type = 'parents') returns Pa(X) \\ {X,Y}", {
  got <- names_of(adjustment_set(adjustment_set_cg, X, Y, type = "parents"))
  expect_setequal(got, c("A", "C"))
})

test_that("adjustment_set(type = 'backdoor') returns a valid backdoor set", {
  Z <- names_of(adjustment_set(adjustment_set_cg, X, Y, type = "backdoor"))
  expect_true(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = Z))
  expect_false("D" %in% Z) # descendant of X should not appear
})

test_that("adjustment_set(type = 'optimal') is valid and minimal here", {
  Z <- names_of(adjustment_set(adjustment_set_cg, X, Y, type = "optimal"))
  expect_true(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = Z))
  expect_true(setequal(Z, "A") || setequal(Z, "K")) # minimal sets in this DAG
})

test_that("is_valid_backdoor works on canonical choices", {
  expect_true(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = A))
  expect_true(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = K))
  expect_false(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = D))
  expect_false(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = c(A, D)))
  expect_false(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = NULL))
})

test_that("all_backdoor_sets enumerates minimal sets", {
  sets <- all_backdoor_sets(adjustment_set_cg, x = X, y = Y)
  norm <- lapply(sets, sort)
  expect_equal(length(norm), 2L)
  expect_true(any(setequal(norm[[1]], "A") | setequal(norm[[2]], "A")))
  expect_true(any(setequal(norm[[1]], "K") | setequal(norm[[2]], "K")))
})

test_that("all_backdoor_sets respects max_size and minimal", {
  sets <- all_backdoor_sets(adjustment_set_cg, x = X, y = Y, minimal = TRUE)
  expect_equal(length(sets), 2L)
  expect_setequal(sets, c("A", "K"))
  sets2 <- all_backdoor_sets(adjustment_set_cg,
    x = X, y = Y, minimal = FALSE,
    max_size = 2
  )
  expect_equal(length(sets2), 5L)
  expect_setequal(sets2, list("A", "K", c("A", "K"), c("C", "A"), c("C", "K")))
})

test_that("all_backdoor_sets includes empty set, if valid", {
  adjustment_set_cg <- caugi_graph(
    C %-->% X,
    X %-->% F,
    X %-->% D,
    A %-->% X,
    A %-->% L, # add v structure, now empty set is valid
    K %-->% L,
    K %-->% Y,
    D %-->% Y,
    D %-->% G,
    Y %-->% H,
    class = "DAG"
  )
  # check if empty set is valid
  expect_true(is_valid_backdoor(adjustment_set_cg, x = X, y = Y, Z = NULL))

  sets <- all_backdoor_sets(adjustment_set_cg,
    x = X, y = Y, minimal = FALSE,
    max_size = 2
  )
  valid_sets <- list(
    character(0), "C", "A", "K", c("A", "K"), c("C", "A"), c("C", "K"),
    c("A", "L"), c("K", "L")
  )
  expect_setequal(sets, valid_sets)
})
