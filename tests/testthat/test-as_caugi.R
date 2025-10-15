test_that("integer adjacency → DAG works and preserves names", {
  m <- matrix(0L, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m["A", "B"] <- 1L
  m["B", "C"] <- 1L

  cg <- as_caugi(m, class = "DAG")

  expect_true(S7::S7_inherits(cg, class = caugi_graph))
  # nodes() should expose vertex names
  expect_setequal(nodes(cg)[["name"]], c("A", "B", "C"))

  e <- as.data.frame(edges(cg))
  expect_setequal(names(e), c("from", "edge", "to"))
  exp <- data.frame(
    from = c("A", "B"),
    edge = c("-->", "-->"),
    to = c("B", "C"),
    stringsAsFactors = FALSE
  )
  expect_equal(e[order(e$from, e$to), ], exp[order(exp$from, exp$to), ])
})

test_that("collapse merges mutual directed edges to undirected", {
  m <- matrix(0L, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m["A", "B"] <- 1L
  m["B", "A"] <- 1L

  cg_no <- as_caugi(m, class = "Unknown", collapse = FALSE, simple = FALSE)
  cg_yes <- as_caugi(m, class = "PDAG", collapse = TRUE)

  e_no <- as.data.frame(edges(cg_no))
  e_yes <- as.data.frame(edges(cg_yes))

  expect_equal(nrow(e_no), 2L)
  expect_true(all(e_no$edge == "-->"))

  expect_equal(nrow(e_yes), 1L)
  expect_equal(e_yes$from, "A")
  expect_equal(e_yes$to, "B")
  expect_equal(e_yes$edge, "---")
})

test_that("matrix validation errors", {
  # non-square
  m_bad <- matrix(0L, 2, 3)
  expect_error(as_caugi(m_bad), "square adjacency matrix", fixed = TRUE)

  # non-integer numeric entries
  x <- matrix(0, 2, 2)
  x[1, 2] <- 0.5
  expect_error(as_caugi(x), "logical or a non-negative integer", fixed = TRUE)

  # invalid codes for non-PAG
  y <- matrix(0L, 2, 2)
  y[1, 2] <- 2L
  expect_error(as_caugi(y, class = "DAG"), "Only 0 and 1 integer codes", fixed = TRUE)

  # invalid codes for PAG
  z <- matrix(0L, 2, 2)
  z[1, 2] <- 4L
  expect_error(as_caugi(z, class = "PAG"), "integer codes 0-3", fixed = TRUE)
})

test_that("logical matrices are accepted for non-PAG and rejected for PAG", {
  m <- matrix(FALSE, 2, 2)
  m[1, 2] <- TRUE

  expect_silent(as_caugi(m, class = "DAG"))

  expect_error(as_caugi(m, class = "PAG"),
    "PAG class is not supported for logical matrices",
    fixed = TRUE
  )
})

test_that("default names V1..Vn when dimnames are missing", {
  m <- matrix(0L, 2, 2)
  m[1, 2] <- 1L
  cg <- as_caugi(m, class = "DAG")
  expect_setequal(nodes(cg)[["name"]], c("V1", "V2"))
})

test_that("PAG code pairs map to expected glyphs", {
  nm <- c("A", "B", "C", "D")
  M <- matrix(0L, 4, 4, dimnames = list(nm, nm))

  # A-B: tail->arrow => A --> B
  M["A", "B"] <- 2L # mark at B end
  M["B", "A"] <- 1L # mark at A end

  # A-C: tail-tail => A --- C
  M["A", "C"] <- 1L
  M["C", "A"] <- 1L

  # B-C: circle->arrow => B o-> C
  M["B", "C"] <- 2L
  M["C", "B"] <- 3L

  # C-D: arrow-arrow => C <-> D
  M["C", "D"] <- 2L
  M["D", "C"] <- 2L

  cg <- as_caugi(M, class = "PAG")
  e <- as.data.frame(edges(cg))[c("from", "edge", "to")]

  exp <- rbind(
    data.frame(from = "A", edge = "-->", to = "B"),
    data.frame(from = "A", edge = "---", to = "C"),
    data.frame(from = "B", edge = "o->", to = "C"),
    data.frame(from = "C", edge = "<->", to = "D")
  )
  ord <- function(df) df[order(df$from, df$edge, df$to), , drop = FALSE]
  expect_equal(ord(e), ord(exp))
})

test_that("double and logical matrices dispatch via integer method", {
  m_d <- matrix(0, 2, 2)
  m_d[1, 2] <- 1
  m_l <- matrix(FALSE, 2, 2)
  m_l[1, 2] <- TRUE

  cg_d <- as_caugi(m_d, class = "DAG")
  cg_l <- as_caugi(m_l, class = "DAG")

  e_d <- as.data.frame(edges(cg_d))
  e_l <- as.data.frame(edges(cg_l))

  expect_equal(e_d$edge, "-->")
  expect_equal(e_l$edge, "-->")
  expect_equal(e_d$from, "V1")
  expect_equal(e_d$to, "V2")
  expect_equal(e_l$from, "V1")
  expect_equal(e_l$to, "V2")
})

test_that("Matrix::Matrix dispatch works", {
  skip_if_not_installed("Matrix")
  m <- Matrix::Matrix(0L, 3, 3, sparse = FALSE)
  dimnames(m) <- list(LETTERS[1:3], LETTERS[1:3])
  m["A", "B"] <- 1L
  m["B", "C"] <- 1L

  cg <- as_caugi(m, class = "DAG")
  e <- as.data.frame(edges(cg))
  expect_setequal(e$edge, c("-->", "-->"))
  expect_setequal(e$from, c("A", "B"))
  expect_setequal(e$to, c("B", "C"))
})

test_that("igraph dispatch works and respects directedness + collapse", {
  skip_if_not_installed("igraph")

  ed <- matrix(c("A", "B", "B", "C"), ncol = 2, byrow = TRUE)

  g_dir <- igraph::graph_from_edgelist(ed, directed = TRUE)
  cg_dir <- as_caugi(g_dir, class = "DAG")
  e_dir <- as.data.frame(edges(cg_dir))
  expect_true(all(e_dir$edge == "-->"))
  expect_setequal(e_dir$from, c("A", "B"))
  expect_setequal(e_dir$to, c("B", "C"))

  g_undir <- igraph::graph_from_edgelist(ed, directed = FALSE)
  cg_undir <- as_caugi(g_undir, class = "Unknown")
  e_un <- as.data.frame(edges(cg_undir))
  expect_true(all(e_un$edge == "---"))

  g_mut <- igraph::graph_from_edgelist(matrix(c("A", "B", "B", "A"), 2, 2, byrow = TRUE),
    directed = TRUE
  )
  cg_col <- as_caugi(g_mut, class = "PDAG", collapse = TRUE)
  e_col <- as.data.frame(edges(cg_col))
  expect_equal(nrow(e_col), 1L)
  expect_equal(e_col$from, "A")
  expect_equal(e_col$to, "B")
  expect_equal(e_col$edge, "---")
})

test_that("graphNEL dispatch works (directed and undirected)", {
  skip_if_not_installed("graph")

  g <- graph::graphNEL(nodes = c("A", "B", "C"), edgemode = "directed")
  g <- graph::addEdge("A", "B", g)
  g <- graph::addEdge("B", "C", g)
  cg <- as_caugi(g, class = "DAG")
  e <- as.data.frame(edges(cg))
  expect_true(all(e$edge == "-->"))
  expect_setequal(e$from, c("A", "B"))
  expect_setequal(e$to, c("B", "C"))

  h <- graph::graphNEL(nodes = c("A", "B"), edgemode = "undirected")
  h <- graph::addEdge("A", "B", h)
  cg_u <- as_caugi(h, class = "Unknown", simple = FALSE)
  eu <- as.data.frame(edges(cg_u))
  expect_equal(nrow(eu), 1L)
  expect_equal(eu$edge, "---")
})

test_that("empty igraph/graphNEL yield empty caugi graphs", {
  skip_if_not_installed("igraph")
  g0 <- igraph::make_empty_graph(n = 0, directed = TRUE)
  cg0 <- as_caugi(g0, class = "DAG")
  expect_equal(nrow(as.data.frame(edges(cg0))), 0L)

  skip_if_not_installed("graph")
  h0 <- graph::graphNEL(nodes = character(), edgemode = "directed")
  cg1 <- as_caugi(h0, class = "DAG")
  expect_equal(nrow(as.data.frame(edges(cg1))), 0L)
})
