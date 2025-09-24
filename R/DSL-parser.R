# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── DSL parser helpers ──────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Is it an edge call / expression?
#'
#' @description This function checks if the expression is an edge call
#'
#' @param expr An expression to check
#'
#' @returns TRUE if the expression is an edge call, FALSE otherwise
#'
#' @keywords internal
.is_edge_call <- function(expr) {
  is.call(expr) && as.character(expr[[1L]]) %in% .edge_ops_get()
}

#' @title Is it a node expr?
#'
#' @description Check if the expression is a valid node expression:
#' symbol, string, number, c(...), +, (...)
#'
#' @param expr An expression to check
#'
#' @returns TRUE if the expression is a valid node expression, FALSE otherwise
#'
#' @keywords internal
.is_node_expr <- function(expr) {
  # symbol or "A"
  if (is.symbol(expr) || (is.character(expr) && length(expr) == 1L)) {
    return(TRUE)
  }

  if (is.call(expr)) {
    fn <- as.character(expr[[1L]])

    # parentheses
    if (fn == "(") {
      return(.is_node_expr(expr[[2L]]))
    }

    # c(A, B + C, (D))
    if (fn == "c") {
      args <- as.list(expr)[-1L]
      return(all(vapply(args, .is_node_expr, logical(1))))
    }

    # A + B + (C + D) — only nodes on both sides
    if (fn == "+") {
      return(.is_node_expr(expr[[2L]]) && .is_node_expr(expr[[3L]]))
    }
  }
  FALSE
}

#' @title Does the expression contain an edge call?
#'
#' @description Recursively check if the expression contains any edge call.
#'
#' @param expr An expression to check
#'
#' @returns TRUE if the expression contains an edge call, FALSE otherwise
#'
#' @keywords internal
.contains_edge <- function(expr) {
  if (.is_edge_call(expr)) {
    return(TRUE)
  }
  if (is.call(expr)) {
    fn <- as.character(expr[[1L]])
    if (fn == "(") {
      return(.contains_edge(expr[[2L]]))
    }
    if (fn == "+") {
      return(.contains_edge(expr[[2L]]) || .contains_edge(expr[[3L]]))
    }
  }
  FALSE
}

#' @title Get glyph for an operator
#'
#' @description Get the glyph string for a given edge operator symbol.
#'
#' @param op_sym A symbol representing the edge operator (e.g., `%-->%`).
#'
#' @returns A string representing the edge glyph (e.g., `"-->"`).
#'
#' @keywords internal
.glyph_of <- function(op_sym) {
  if (is.null(op_sym)) {
    return(NULL)
  }
  key <- as.character(op_sym)
  m <- .glyph_map_get()
  if (is.null(m)) {
    return(NULL)
  }
  nms <- names(m)
  if (is.null(nms) || !(key %in% nms)) {
    return(NULL)
  }
  m[[key]]
}

#' @title Expand node expressions
#'
#' @description Expand node expressions: symbol, "B", c(...), +, (...)
#'
#' @param expr An expression representing nodes.
#'
#' @returns A character vector of node names.
#'
#' @keywords internal
.expand_nodes <- function(expr) {
  if (is.symbol(expr)) {
    return(deparse1(expr))
  }
  if (is.character(expr) && length(expr) == 1L) {
    return(expr)
  }
  if (is.numeric(expr) && length(expr) == 1L) {
    return(as.character(expr))
  }

  if (is.call(expr)) {
    fn <- as.character(expr[[1L]])
    if (fn == "(") {
      return(.expand_nodes(expr[[2L]]))
    }
    if (fn == "+") {
      return(c(.expand_nodes(expr[[2L]]), .expand_nodes(expr[[3L]])))
    }
    if (fn == "c") {
      args <- as.list(expr)[-1L]
      return(unlist(lapply(args, .expand_nodes), use.names = FALSE))
    }
  }

  stop("Unsupported node expression: ", deparse1(expr), call. = FALSE)
}


#' @title Expand target expressions with `=`
#'
#' @description Split any expression into top-level '+' terms (fully flattened).
#'
#' @param expr An expression representing nodes.
#'
#' @returns A character vector of node names.
#'
#' @keywords internal
.split_plus <- function(expr) {
  if (is.call(expr)) {
    fn <- as.character(expr[[1L]])
    if (fn == "+") {
      return(c(.split_plus(expr[[2L]]), .split_plus(expr[[3L]])))
    }
    if (fn == "(") {
      return(.split_plus(expr[[2L]]))
    }
  }
  list(expr)
}

#' @title Combine terms with '+'
#'
#' @description Combine a list of terms into a single left-associative '+' call.
#'
#' @param terms A list of expressions to combine.
#'
#' @returns A single expression combining the terms with '+'.
#'
#' @keywords internal
.combine_plus <- function(terms) {
  len <- length(terms)
  if (len == 0) {
    return(NULL)
  }
  if (len == 1) {
    return(terms[[1L]])
  }
  out <- terms[[1L]]
  for (i in 2:len) out <- call("+", out, terms[[i]])
  out
}

#' @title Parse one caugi_graph(...) argument
#'
#' @description Parse one caugi_graph(...) argument into edge units with L/R context.
#'
#' @param expr An expression representing an edge with nodes
#'
#' @returns A list of edge units, each with lhs, rhs, and glyph.
#'
#' @keywords internal
.parse_edge_arg <- function(expr) {
  terms <- .split_plus(expr)

  is_edge <- vapply(terms, .is_edge_call, TRUE)
  if (!any(is_edge)) {
    stop("Expected an edge expression; got: ",
      deparse(expr),
      call. = FALSE
    )
  }

  edge_idx <- which(is_edge)
  units <- vector("list", length(edge_idx))

  keep_node <- function(x) !is.null(x) && !.is_edge_call(x)
  for (k in seq_along(edge_idx)) {
    i <- edge_idx[k]
    edge_term <- terms[[i]]

    lhs <- edge_term[[2L]]
    rhs <- edge_term[[3L]]

    prev_idx <- if (k > 1L) edge_idx[k - 1L] else 0L
    next_idx <- if (k < length(edge_idx)) edge_idx[k + 1L] else length(terms) + 1L

    left_idx <- if (prev_idx + 1L <= i - 1L) seq.int(prev_idx + 1L, i - 1L) else integer(0)
    right_idx <- if (i + 1L <= next_idx - 1L) seq.int(i + 1L, next_idx - 1L) else integer(0)

    left_nodes_terms <- Filter(keep_node, terms[left_idx])
    right_nodes_terms <- Filter(keep_node, terms[right_idx])

    if (length(left_nodes_terms)) {
      lhs <- call("+", .combine_plus(left_nodes_terms), lhs)
    }
    if (length(right_nodes_terms)) {
      rhs <- call("+", rhs, .combine_plus(right_nodes_terms))
    }

    units[[k]] <- list(lhs = lhs, rhs = rhs, glyph = .glyph_of(edge_term[[1L]]))
  }
  units
}


#' @title Turn edge units into a tibble of edges
#'
#' @description Convert a list of edge units into a tibble with columns
#' `from`, `edge`, and `to`.
#'
#' @param units A list of edge units, each with `lhs`, `rhs`, and `glyph`.
#'
#' @returns A tibble with columns `from`, `edge`, and `to`.
#'
#' @keywords internal
.edge_units_to_tibble <- function(units) {
  dfs <- lapply(units, function(u) {
    froms <- .expand_nodes(u$lhs)
    tos <- .expand_nodes(u$rhs)
    tibble::tibble(
      from = rep(froms, each = length(tos)),
      edge = u$glyph,
      to = rep(tos, times = length(froms))
    )
  })
  dplyr::bind_rows(dfs)
}

#' @title Collect edges and nodes
#'
#' @description Collect edges (via .parse_edge_arg) and explicitly declared
#' nodes (no edges).
#'
#' @param calls A list of expressions from caugi_graph(...)
#'
#' @returns A list with two elements:
#' * edges: a tibble with columns `from`, `edge`, `to`
#' * declared: a character vector of explicitly declared nodes
#'
#' @keywords internal
.collect_edges_nodes <- function(calls) {
  units <- list()
  declared <- character()
  for (ex in calls) {
    if (.contains_edge(ex)) {
      units <- c(units, .parse_edge_arg(ex))
    } else if (.is_node_expr(ex)) {
      declared <- c(declared, .expand_nodes(ex))
    } else {
      stop("Expected an edge (A %-->% B) ",
        "or a node expression (C, D, C+D, c(C,D)); got: ",
        deparse(ex),
        call. = FALSE
      )
    }
  }
  edges <- if (length(units)) {
    dplyr::distinct(.edge_units_to_tibble(units))
  } else {
    tibble::tibble(from = character(), to = character(), edge = character())
  }
  list(edges = edges, declared = unique(declared))
}
