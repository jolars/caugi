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

test_that("empty caugi graph initialization works", {
  cg <- caugi_graph()
  expect_s3_class(cg, "caugi_graph")
  expect_equal(length(cg), 0)
  expect_equal(nrow(cg$edges), 0)
  expect_equal(length(cg), 0)
})

test_that("building DAG with undirected edges results in error", {
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
