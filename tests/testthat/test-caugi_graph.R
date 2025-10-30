# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── caugi graph tests ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────────── Length ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("caugi graph length is correct", {
  cg <- caugi(
    A %-->% B,
    B %---% C,
    C %<->% D,
    D %--o% E,
    E %o->% F,
    F %o-o% A
  )
  expect_equal(length(cg), 6)

  cg <- caugi(
    A %-->% B,
    B %-->% C,
    C %-->% D
  )
  expect_equal(length(cg), 4)

  cg <- caugi(
    A %---% B
  )
  expect_equal(length(cg), 2)

  cg <- caugi()
  expect_equal(length(cg), 0)
})

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Initialization ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("caugi graph generation works as expected", {
  cg <- caugi(
    A %-->% B,
    B %---% C,
    C %<->% D,
    D %--o% E,
    E %o->% F,
    F %o-o% A
  )
  expect_s7_class(cg, caugi)
  expect_equal(nrow(cg@nodes), 6)
  expect_equal(nrow(cg@edges), 6)
  expect_true(all(c("from", "edge", "to") %in% names(cg@edges)))
  expect_true(all(c("name") %in% names(cg@nodes)))
  expect_equal(sort(cg@nodes$name), sort(LETTERS[1:6]))
  expect_equal(
    sort(cg@edges$edge),
    sort(c("o->", "--o", "o-o", "-->", "<->", "---"))
  )
})

test_that("empty caugi graph initialization works", {
  cg <- caugi()
  expect_s7_class(cg, caugi)
  expect_equal(length(cg), 0)
  expect_equal(nrow(cg@edges), 0)
  expect_equal(length(cg), 0)
})


test_that("building graph with invalid class results in error", {
  expect_error(
    caugi(
      A %-->% B,
      B %---% C,
      class = "INVALID"
    )
  )
})

test_that("building a graph with duplicates will deduplicate on initial call", {
  cg <- caugi(
    A %-->% B,
    A %-->% B,
    B %---% C,
    B %---% C,
    class = "PDAG"
  )

  cg_equiv <- caugi(
    A %-->% B,
    B %---% C,
    class = "PDAG"
  )
  expect_equal(cg, cg_equiv)
})

# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Simple / non-simple ──────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("building graph with simple = FALSE needs class = UNKNOWN", {
  expect_error(
    caugi(
      A %-->% B,
      B %---% C,
      class = "DAG",
      simple = FALSE
    )
  )
  expect_error(
    caugi(
      A %-->% B,
      B %---% C,
      class = "PDAG",
      simple = FALSE
    )
  )
  expect_s7_class(
    caugi(
      A %-->% B,
      B %---% C,
      class = "UNKNOWN",
      simple = FALSE
    ),
    caugi
  )
})

test_that("non-simple graphs allows self loops and parallel edges", {
  expect_s7_class(
    caugi(
      A %-->% A,
      B %---% C,
      class = "UNKNOWN",
      simple = FALSE
    ),
    caugi
  )
  expect_s7_class(
    caugi(
      A %-->% B,
      A %<->% B,
      class = "UNKNOWN",
      simple = FALSE
    ),
    caugi
  )

  expect_s7_class(
    caugi(
      A %-->% B,
      A %o->% A,
      class = "UNKNOWN",
      simple = FALSE
    ),
    caugi
  )
})

test_that("building graph with simple = TRUE disallows parallel edges", {
  expect_error(
    caugi(
      A %-->% B,
      A %---% B,
      class = "UNKNOWN",
      simple = TRUE
    )
  )
  expect_s7_class(
    caugi(
      A %-->% B,
      B %---% C,
      class = "UNKNOWN",
      simple = TRUE
    ),
    caugi
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── DAG tests ───────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("building DAG with non-directed edges results in error", {
  expect_error(
    caugi(
      A %-->% B,
      B %---% C,
      class = "DAG"
    )
  )
  expect_error(
    caugi(
      A %-->% B,
      B %o->% C,
      class = "DAG"
    )
  )
})

test_that("building DAG with cycle results in error", {
  expect_error(
    caugi(
      A %-->% B,
      B %-->% C,
      C %-->% A,
      class = "DAG"
    )
  )
  cg <- caugi(A %-->% B, class = "DAG")
  expect_error(
    cg |> add_edge(B %-->% A) |> build()
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── PDAG tests ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("building PDAG with directed cycle results in error", {
  expect_error(
    caugi(
      A %-->% B,
      B %-->% C,
      C %-->% A,
      class = "PDAG"
    )
  )
  cg <- caugi(A %-->% B, class = "PDAG")
  expect_error(
    cg |> add_edge(B %-->% A) |> build()
  )
})

test_that("building PDAG with bidirected edges results in error", {
  expect_error(
    caugi(
      A %-->% B,
      B %<->% C,
      class = "PDAG"
    )
  )
  expect_error(
    caugi(
      A %-->% B,
      B %o-o% C,
      class = "PDAG"
    )
  )
  cg <- caugi(A %-->% B, class = "PDAG")
  expect_error(
    cg |> add_edge(B %o->% C) |> build()
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────── Standard evaluation input ───────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("caugi builds from parallel vectors", {
  cg_vec <- caugi(
    from = c("A", "B", "C"),
    edge = c("-->", "---", "<->"),
    to   = c("B", "C", "D")
  )

  expect_s7_class(cg_vec, caugi)
  expect_equal(sort(cg_vec@nodes$name), sort(c("A", "B", "C", "D")))
  expect_equal(nrow(cg_vec@edges), 3)
  expect_true(all(c("from", "edge", "to") %in% names(cg_vec@edges)))

  cg_expr <- caugi(
    A %-->% B,
    B %---% C,
    C %<->% D
  )
  expect_equal(cg_vec, cg_expr)
})

test_that("caugi forbids mixing ... with from/edge/to", {
  expect_error(
    caugi(
      A %-->% B,
      from = "C", edge = "-->", to = "D"
    ),
    "Provide edges via infix expressions"
  )
})

test_that("caugi requires from, edge, to all present and equal length", {
  # missing one
  expect_error(
    caugi(from = "A", edge = "-->"),
    "`from`, `edge`, `to` must all be supplied"
  )

  # length mismatch
  expect_error(
    caugi(
      from = c("A", "B"),
      edge = c("-->", "---"),
      to   = "C"
    ),
    "`from`, `edge`, `to` must be equal length"
  )
})

test_that("caugi with empty vectors yields empty graph", {
  cg <- caugi(from = character(), edge = character(), to = character())
  expect_s7_class(cg, caugi)
  expect_equal(length(cg), 0)
  expect_equal(nrow(cg@edges), 0)
  expect_equal(nrow(cg@nodes), 0)
})

test_that("caugi(vector mode) gets the same result as in with DSL", {
  cg1 <- caugi(
    from = c("A"),
    edge = c("-->"),
    to   = c("B")
  )
  cg2 <- caugi(A %-->% B)
  expect_equal(cg1, cg2)
})

test_that("caugi(vector mode) respects class and simple rules", {
  # valid PDAG
  expect_s7_class(
    caugi(
      from = c("A", "B"),
      edge = c("-->", "---"),
      to = c("B", "C"),
      class = "PDAG"
    ),
    caugi
  )

  # invalid DAG due to non-directed edge
  expect_error(
    caugi(
      from = c("A", "B"),
      edge = c("-->", "---"),
      to = c("B", "C"),
      class = "DAG"
    )
  )

  # simple = TRUE disallows parallel edges under UNKNOWN
  expect_error(
    caugi(
      from = c("A", "A"),
      edge = c("-->", "<->"),
      to = c("B", "B"),
      class = "UNKNOWN",
      simple = TRUE
    )
  )
})

test_that("caugi(vector mode) works with isolated nodes", {
  cg_vec_1 <- caugi(
    from = c("A", "B", "C"),
    edge = c("-->", "---", "<->"),
    to = c("B", "C", "D"),
    nodes = c("A", "B", "C", "D", "E", "F")
  )
  cg_vec_2 <- caugi(
    from = c("A", "B", "C"),
    edge = c("-->", "---", "<->"),
    to = c("B", "C", "D"),
    nodes = c("E", "F")
  )
  expect_equal(cg_vec_1, cg_vec_2)
})

test_that("caugi preserves node order from nodes parameter", {
  # When nodes parameter is provided, its order should be preserved
  # even when edges reference nodes in different order
  cg <- caugi(
    from = c("V3", "V7", "V2"),
    edge = c("-->", "-->", "-->"),
    to = c("V1", "V9", "V5"),
    nodes = paste0("V", 1:10),
    class = "DAG"
  )

  # Node order should match the provided nodes parameter
  expect_equal(V(cg)$name, paste0("V", 1:10))

  # Edges should still be present and correct
  expect_equal(nrow(edges(cg)), 3)
})

# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Errors and warnings ──────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────


test_that("caugi errors with trailing commas", {
  expect_error(
    caugi(
      A %-->% B,
      B %---% C,
    ),
    "Argument 3 is missing"
  )
})

test_that("caugi warns when build = TRUE for empty graph", {
  expect_warning(
    caugi(build = TRUE),
    "No edges or nodes provided; graph will not be built."
  )
})

test_that("caugi with wrong node input errors", {
  expect_error(
    caugi(
      from = c("A", "B"),
      edge = c("-->", "---"),
      to = c("B", "C"),
      nodes = list("D", "E")
    ),
    "`nodes` must be a character vector"
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── .view_to_caugi ─────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that(".view_to_caugi works as expected", {
  cg <- caugi(
    A %-->% B,
    B %---% C,
    C %<->% D
  )

  cg2 <- .view_to_caugi(cg@ptr)
  expect_s7_class(cg2, caugi)
  expect_equal(cg, cg2)
})

test_that(".view_to_caugi fails on NULL ptr", {
  expect_error(
    .view_to_caugi(NULL),
    "ptr is NULL"
  )
})

test_that(".view_to_caugi fails on faulty node_names", {
  cg <- caugi(
    A %-->% B
  )
  expect_error(
    .view_to_caugi(cg@ptr, node_names = c("A")),
    "length"
  )
})

test_that(".view_to_caugi works for empty cg", {
  cg <- caugi(A, B, build = TRUE)
  expect_equal(cg, .view_to_caugi(cg@ptr))
})

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────── Freezing and unfreezing ────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("freeze / unfreeze works as expected", {
  cg <- caugi(
    A %-->% B,
    B %---% C
  )

  # test if built is TRUE
  expect_true(cg@built)
  # currently frozen
  s <- cg@.state
  expect_error(s$built <- FALSE)

  s <- caugi:::.unfreeze_state(cg@.state)
  s$built <- FALSE
  expect_false(s$built)
  expect_false(cg@.state$built)
})
