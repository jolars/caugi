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

test_that("normalize_latent_structure matches R reference implementation", {
  normalize_reference_r <- function(cg, latents) {
    latents <- unique(latents)
    if (length(latents) == 0L) {
      return(cg)
    }
    cg <- exogenize(cg, nodes = latents)

    changed <- TRUE
    while (changed) {
      changed <- FALSE
      current_latents <- intersect(latents, nodes(cg)$name)
      if (length(current_latents) == 0L) {
        break
      }

      child_counts <- vapply(
        current_latents,
        function(l) {
          ch <- children(cg, l)
          if (is.null(ch)) 0L else length(ch)
        },
        integer(1)
      )
      to_drop <- current_latents[child_counts <= 1L]
      if (length(to_drop) > 0L) {
        cg <- remove_nodes(cg, name = to_drop)
        changed <- TRUE
        next
      }

      current_latents <- intersect(latents, nodes(cg)$name)
      if (length(current_latents) < 2L) {
        break
      }

      child_sets <- lapply(
        current_latents,
        function(l) {
          ch <- children(cg, l)
          if (is.null(ch)) character(0) else sort(unique(ch))
        }
      )

      drop_one <- NULL
      for (i in seq_len(length(current_latents) - 1L)) {
        for (j in (i + 1L):length(current_latents)) {
          ch_i <- child_sets[[i]]
          ch_j <- child_sets[[j]]
          if (length(ch_i) < length(ch_j) && all(ch_i %in% ch_j)) {
            drop_one <- current_latents[i]
            break
          }
          if (length(ch_j) < length(ch_i) && all(ch_j %in% ch_i)) {
            drop_one <- current_latents[j]
            break
          }
        }
        if (!is.null(drop_one)) {
          break
        }
      }

      if (!is.null(drop_one)) {
        cg <- remove_nodes(cg, name = drop_one)
        changed <- TRUE
      }
    }

    cg
  }

  set.seed(42)
  dag <- generate_graph(n = 80, m = 8, class = "DAG")
  latents <- sample(nodes(dag)$name, size = 12)

  rust_norm <- normalize_latent_structure(dag, latents = latents)
  ref_norm <- normalize_reference_r(dag, latents = latents)

  expect_equal(shd(rust_norm, ref_norm), 0)
  expect_setequal(nodes(rust_norm)$name, nodes(ref_norm)$name)
})

test_that("normalize_latent_structure validation and loop branches are covered", {
  cg_pdag <- caugi(A %---% B, class = "PDAG")
  expect_error(
    normalize_latent_structure(cg_pdag, latents = "A"),
    "normalize_latent_structure\\(\\) can only be applied to DAGs\\."
  )

  cg <- caugi(U, A %-->% B, class = "DAG")
  expect_error(
    normalize_latent_structure(cg, latents = 1),
    "`latents` must be a character vector of node names."
  )

  expect_identical(normalize_latent_structure(cg, latents = character(0)), cg)

  expect_error(
    normalize_latent_structure(cg, latents = "Z"),
    "Unknown latent node\\(s\\): Z"
  )

  # Latent with no children exercises the NULL->0 branch.
  out_no_children <- normalize_latent_structure(cg, latents = "U")
  expect_false("U" %in% out_no_children@nodes$name)

  # Nested child set branch (drop_one <- current_latents[i]).
  dag_nested <- caugi(
    U1 %-->% X + Y,
    U2 %-->% X + Y + Z,
    X %-->% W,
    class = "DAG"
  )
  out_nested <- normalize_latent_structure(dag_nested, latents = c("U1", "U2"))
  expect_false(
    "U1" %in% out_nested@nodes$name && "U2" %in% out_nested@nodes$name
  )

  # Empty/overlap branch behavior.
  overlap <- normalize_latent_structure(
    caugi(U1 %-->% X + Y, U2 %-->% X + Y, class = "DAG"),
    latents = c("U1", "U2")
  )
  expect_true(all(c("U1", "U2") %in% nodes(overlap)$name))
})
