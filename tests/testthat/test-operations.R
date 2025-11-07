# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Skeleton ───────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("DAG --> UG with skeleton keeps the skeleton", {
  cg <- caugi(
    A %-->% B,
    B %-->% C,
    C %-->% D,
    D %-->% E,
    F, G,
    class = "DAG"
  )

  skel_cg <- skeleton(cg)

  expect_equal(edge_types(skel_cg), "---")
  expect_equal(length(skel_cg), 7)
  expect_equal(nodes(cg), nodes(skel_cg))
  expect_equal(edges(cg)$from, edges(skel_cg)$from)
  expect_equal(edges(cg)$to, edges(skel_cg)$to)
})

test_that("PDAG --> UG with skeleton keeps the skeleton", {
  cg <- caugi(
    A %-->% B,
    B %-->% C,
    C %---% D,
    D %-->% E,
    F, G,
    class = "PDAG"
  )

  skel_cg <- skeleton(cg)

  expect_equal(edge_types(skel_cg), "---")
  expect_equal(length(skel_cg), 7)
  expect_equal(nodes(cg), nodes(skel_cg))
  expect_equal(edges(cg)$from, edges(skel_cg)$from)
  expect_equal(edges(cg)$to, edges(skel_cg)$to)
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── Moralizing ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("moralize works on DAGs", {
  cg <- caugi(
    A %-->% B,
    B %-->% C,
    D %-->% C,
    E %-->% C,
    D %-->% B,
    F, G,
    nodes = c("A", "B", "C", "D", "E", "F", "G"),
    class = "DAG"
  )

  moralized_cg <- moralize(cg)

  expect_equal(edge_types(moralized_cg), "---")
  expect_equal(length(moralized_cg), 7)
  expect_equal(nodes(cg), nodes(moralized_cg))
  expect_equal(moralized_cg@graph_class, "UG")

  expected_moral_cg <- caugi(
    A %---% B,
    A %---% D,
    B %---% C,
    B %---% D,
    B %---% E,
    C %---% D,
    C %---% E,
    D %---% E,
    F, G,
    nodes = c("A", "B", "C", "D", "E", "F", "G"),
    class = "UG"
  )
  expect_equal(edges(moralized_cg), edges(expected_moral_cg))
})

test_that("moralize fails on non-DAGs", {
  cg_pdag <- caugi(
    A %-->% B,
    B %-->% C,
    C %---% D,
    class = "PDAG"
  )

  expect_error(
    moralize(cg_pdag),
    "moralize\\(\\) can only be applied to DAGs\\."
  )

  cg_ug <- caugi(
    A %---% B,
    B %---% C,
    class = "UG"
  )

  expect_error(
    moralize(cg_ug),
    "moralize\\(\\) can only be applied to DAGs\\."
  )
})
