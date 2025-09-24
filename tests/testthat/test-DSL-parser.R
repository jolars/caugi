# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── DSL parser tests ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that("local edge/glyph registries exist for tests", {
  # Minimal registries for tests
  .edge_ops_get <<- function() c("%-->%", "%<--%")
  .glyph_map_get <<- function() list("%-->%" = "-->", "%<--%" = "<--")

  expect_type(.edge_ops_get(), "character")
  expect_true("%-->%" %in% .edge_ops_get())
  expect_equal(.glyph_map_get()[["%-->%"]], "-->")
})

# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────── is functions ─────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that(".is_edge_call identifies edge calls", {
  edge_call <- quote(A %-->% B) # parsing only; no eval
  non_edge <- quote(A + B)

  expect_true(.is_edge_call(edge_call))
  expect_false(.is_edge_call(non_edge))
  expect_false(.is_edge_call(quote(A)))
})

test_that(".is_node_expr validates allowed node expressions", {
  expect_true(.is_node_expr(quote(A)))
  expect_true(.is_node_expr("A"))
  expect_true(.is_node_expr(quote((A))))
  expect_true(.is_node_expr(quote(c(A, "B", (C)))))
  expect_true(.is_node_expr(quote(A + (B + C))))
  # Edge calls are not node expressions
  expect_false(.is_node_expr(quote(A %-->% B)))
})

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── glyph of ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that(".glyph_of returns correct glyph", {
  expect_equal(.glyph_of(as.symbol("%-->%")), "-->")
  expect_equal(.glyph_of(as.symbol("%---%")), "---")
  expect_equal(.glyph_of(as.symbol("%<->%")), "<->")
  expect_equal(.glyph_of(as.symbol("%--o%")), "--o")
  expect_equal(.glyph_of(as.symbol("%o-o%")), "o-o")
  expect_equal(.glyph_of(as.symbol("%o->%")), "o->")

  # new registered edge
  register_caugi_edge(
    glyph = "<--",
    tail_mark = "arrow",
    head_mark = "tail",
    class = "directed",
    symmetric = FALSE,
    flags = "TRAVERSABLE_WHEN_CONDITIONED"
  )
  expect_equal(.glyph_of(as.symbol("%<--%")), "<--")
})

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────── expand, split, parse, collect ─────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

test_that(".expand_nodes expands symbols, strings, numerics, +, c(), and ()", {
  expect_equal(.expand_nodes(quote(A)), "A")
  expect_equal(.expand_nodes("B"), "B")
  expect_equal(.expand_nodes(1), "1")
  expect_equal(.expand_nodes(quote((A))), "A")
  expect_equal(
    .expand_nodes(quote(A + B + (C))),
    c("A", "B", "C")
  )
  expect_equal(
    .expand_nodes(quote(c(A, "B", (C), 3))),
    c("A", "B", "C", "3")
  )
  expect_error(.expand_nodes(quote(A:B)))
})

test_that(".split_plus and .combine_plus are consistent", {
  expr <- quote(A + (B + C) + D)
  parts <- .split_plus(expr)

  # note: middle stays nested
  expect_length(parts, 4)
  expect_equal(lapply(parts, deparse1), list("A", "B", "C", "D"))

  # Recombine flattened parts explicitly
  recombined <- .combine_plus(list(quote(A), quote(B), quote(C), quote(D)))
  expect_equal(deparse1(recombined), "A + B + C + D")
})

test_that(".parse_edge_arg extracts context correctly", {
  # Build A + (B %-->% C) + D using calls to avoid precedence concerns
  edge_core <- call("%-->%", quote(B), quote(C))
  expr <- call("+", quote(A), call("+", edge_core, quote(D)))

  units <- .parse_edge_arg(expr)
  expect_length(units, 1)

  u <- units[[1]]
  # LHS should be A + B; RHS should be C + D
  expect_equal(.expand_nodes(u$lhs), c("A", "B"))
  expect_equal(.expand_nodes(u$rhs), c("C", "D"))
  expect_equal(u$glyph, "-->")
})

test_that(".edge_units_to_tibble produces cartesian edges", {
  edge_unit <- list(
    lhs = quote(A + B),
    rhs = quote(C + D),
    glyph = "-->"
  )
  df <- .edge_units_to_tibble(list(edge_unit))
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 4)
  expect_equal(df$from, c("A", "A", "B", "B"))
  expect_equal(df$to, c("C", "D", "C", "D"))
  expect_true(all(df$edge == "-->"))
})

test_that(".collect_edges_nodes gathers edges and declared nodes", {
  # Expressions:
  #   1) A + B %-->% C + D
  #   2) c(E, "F", (G))        # declared nodes
  #   3) Duplicate edge to test distinct
  ex1 <- quote(A + B %-->% C + D)
  ex_decl <- call("c", quote(E), "F", quote((G)))
  ex_dup <- quote(B %-->% C)

  out <- .collect_edges_nodes(list(ex1, ex_decl, ex_dup))

  # Declared nodes unique
  expect_setequal(out$declared, c("E", "F", "G"))

  # Edges distinct and complete cartesian from {A,B} to {C,D}
  # plus B->C duplicate removed
  df <- out$edges
  expect_s3_class(df, "tbl_df")
  expect_true(all(c("from", "edge", "to") %in% names(df)))
  expect_equal(nrow(df), 4) # A->C, A->D, B->C, B->D
  expect_true(any(df$from == "A" & df$to == "C"))
  expect_true(any(df$from == "A" & df$to == "D"))
  expect_true(any(df$from == "B" & df$to == "C"))
  expect_true(any(df$from == "B" & df$to == "D"))
  expect_true(all(df$edge == "-->"))
})
