test_that("normalize_latent_structure exogenizes latents (Lemma 1)", {
  dag <- caugi(
    A %-->% U,
    U %-->% X + Y,
    class = "DAG"
  )

  norm <- normalize_latent_structure(dag, latents = "U")

  expect_null(parents(norm, "U"))
  expect_setequal(children(norm, "A"), c("X", "Y"))

  admg1 <- latent_project(dag, latents = "U")
  admg2 <- latent_project(norm, latents = "U")
  expect_equal(shd(admg1, admg2), 0)
})

test_that("normalize_latent_structure removes singleton latents (Lemma 2)", {
  dag <- caugi(
    U %-->% X,
    class = "DAG"
  )

  norm <- normalize_latent_structure(dag, latents = "U")

  expect_false("U" %in% nodes(norm)$name)
  expect_setequal(nodes(norm)$name, "X")

  admg1 <- latent_project(dag, latents = "U")
  admg2 <- latent_project(norm, latents = character(0))
  expect_equal(shd(admg1, admg2), 0)
})

test_that("normalize_latent_structure drops latent with one child even if it has descendants", {
  dag <- caugi(
    U %-->% X,
    X %-->% Y,
    class = "DAG"
  )

  norm <- normalize_latent_structure(dag, latents = "U")

  expect_false("U" %in% nodes(norm)$name)
  expect_setequal(nodes(norm)$name, c("X", "Y"))
  expect_equal(children(norm, "X"), "Y")

  admg1 <- latent_project(dag, latents = "U")
  admg2 <- latent_project(norm, latents = character(0))
  expect_equal(shd(admg1, admg2), 0)
})

test_that("normalize_latent_structure removes nested latents (Lemma 3.8)", {
  dag <- caugi(
    U %-->% X + Y + Z,
    W %-->% X + Y,
    class = "DAG"
  )

  norm <- normalize_latent_structure(dag, latents = c("U", "W"))

  expect_true("U" %in% nodes(norm)$name)
  expect_false("W" %in% nodes(norm)$name)

  admg1 <- latent_project(dag, latents = c("U", "W"))
  admg2 <- latent_project(norm, latents = "U")
  expect_equal(shd(admg1, admg2), 0)
})

test_that("normalize_latent_structure does not change overlapping but non-nested latents", {
  dag <- caugi(
    U1 %-->% A + B + C,
    U2 %-->% B + D,
    class = "DAG"
  )

  norm <- normalize_latent_structure(dag, latents = c("U1", "U2"))

  expect_equal(shd(dag, norm), 0)
  expect_setequal(nodes(norm)$name, c("U1", "U2", "A", "B", "C", "D"))
})

test_that("normalize_latent_structure is idempotent", {
  dag <- caugi(
    A %-->% U,
    U %-->% X + Y,
    V %-->% X + Y,
    class = "DAG"
  )

  norm1 <- normalize_latent_structure(dag, latents = c("U", "V"))
  norm2 <- normalize_latent_structure(norm1, latents = c("U", "V"))

  expect_setequal(nodes(norm1)$name, nodes(norm2)$name)
  expect_equal(shd(norm1, norm2), 0)
})
