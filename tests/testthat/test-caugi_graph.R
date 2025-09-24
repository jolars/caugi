# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── caugi graph tests ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────────── Length ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("caugi graph length is correct", {
  cg <- caugi_graph(
    A %-->% B,
    B %---% C,
    C %<->% D,
    D %--o% E,
    E %o->% F,
    F %o-o% A
  )
  expect_equal(length(cg), 6)

  cg <- caugi_graph(
    A %-->% B,
    B %-->% C,
    C %-->% D
  )
  expect_equal(length(cg), 4)

  cg <- caugi_graph(
    A %---% B
  )
  expect_equal(length(cg), 2)

  cg <- caugi_graph()
  expect_equal(length(cg), 0)
})

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────── Initialization ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("caugi graph generation works as expected", {
  cg <- caugi_graph(
    A %-->% B,
    B %---% C,
    C %<->% D,
    D %--o% E,
    E %o->% F,
    F %o-o% A
  )
  expect_s3_class(cg, "caugi_graph")
  expect_equal(nrow(cg$nodes), 6)
  expect_equal(nrow(cg$edges), 6)
  expect_true(all(c("from", "edge", "to") %in% names(cg$edges)))
  expect_true(all(c("name") %in% names(cg$nodes)))
  expect_equal(sort(cg$nodes$name), sort(LETTERS[1:6]))
  expect_equal(
    sort(cg$edges$edge),
    sort(c("o->", "--o", "o-o", "-->", "<->", "---"))
  )
})

test_that("empty caugi graph initialization works", {
  cg <- caugi_graph()
  expect_s3_class(cg, "caugi_graph")
  expect_equal(length(cg), 0)
  expect_equal(nrow(cg$edges), 0)
  expect_equal(length(cg), 0)
})


test_that("building graph with invalid class results in error", {
  expect_error(
    caugi_graph(
      A %-->% B,
      B %---% C,
      class = "INVALID"
    )
  )
})

test_that("building a graph with duplicates will deduplicate on initial call", {
  cg <- caugi_graph(
    A %-->% B,
    A %-->% B,
    B %---% C,
    B %---% C,
    class = "PDAG"
  )

  cg_equiv <- caugi_graph(
    A %-->% B,
    B %---% C,
    class = "PDAG"
  )
  expect_equal(cg, cg_equiv)
})

# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────── Simple / non-simple ──────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("building graph with simple = FALSE needs class = Unknown", {
  expect_error(
    caugi_graph(
      A %-->% B,
      B %---% C,
      class = "DAG",
      simple = FALSE
    )
  )
  expect_error(
    caugi_graph(
      A %-->% B,
      B %---% C,
      class = "PDAG",
      simple = FALSE
    )
  )
  expect_s3_class(
    caugi_graph(
      A %-->% B,
      B %---% C,
      class = "Unknown",
      simple = FALSE
    ),
    "caugi_graph"
  )
})

test_that("non-simple graphs allows self loops and parallel edges", {
  expect_s3_class(
    caugi_graph(
      A %-->% A,
      B %---% C,
      class = "Unknown",
      simple = FALSE
    ),
    "caugi_graph"
  )
  expect_s3_class(
    caugi_graph(
      A %-->% B,
      A %<->% B,
      class = "Unknown",
      simple = FALSE
    ),
    "caugi_graph"
  )

  expect_s3_class(
    caugi_graph(
      A %-->% B,
      A %o->% A,
      class = "Unknown",
      simple = FALSE
    ),
    "caugi_graph"
  )
})

test_that("building graph with simple = TRUE disallows parallel edges", {
  expect_error(
    caugi_graph(
      A %-->% B,
      A %---% B,
      class = "Unknown",
      simple = TRUE
    )
  )
  expect_s3_class(
    caugi_graph(
      A %-->% B,
      B %---% C,
      class = "Unknown",
      simple = TRUE
    ),
    "caugi_graph"
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── DAG tests ───────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("building DAG with non-directed edges results in error", {
  expect_error(
    caugi_graph(
      A %-->% B,
      B %---% C,
      class = "DAG"
    )
  )
  expect_error(
    caugi_graph(
      A %-->% B,
      B %o->% C,
      class = "DAG"
    )
  )
})

test_that("building DAG with cycle results in error", {
  expect_error(
    caugi_graph(
      A %-->% B,
      B %-->% C,
      C %-->% A,
      class = "DAG"
    )
  )
  cg <- caugi_graph(A %-->% B, class = "DAG")
  expect_error(
    cg |> add_edge(B %-->% A) |> build()
  )
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── PDAG tests ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("building PDAG with directed cycle results in error", {
  expect_error(
    caugi_graph(
      A %-->% B,
      B %-->% C,
      C %-->% A,
      class = "PDAG"
    )
  )
  cg <- caugi_graph(A %-->% B, class = "PDAG")
  expect_error(
    cg |> add_edge(B %-->% A) |> build()
  )
})

test_that("building PDAG with bidirected edges results in error", {
  expect_error(
    caugi_graph(
      A %-->% B,
      B %<->% C,
      class = "PDAG"
    )
  )
  expect_error(
    caugi_graph(
      A %-->% B,
      B %o-o% C,
      class = "PDAG"
    )
  )
  cg <- caugi_graph(A %-->% B, class = "PDAG")
  expect_error(
    cg |> add_edge(B %o->% C) |> build()
  )
})
