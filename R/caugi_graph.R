#' @useDynLib caugi, .registration = TRUE
#' @importFrom cpp11 cpp_register
#' @importFrom tibble tibble as_tibble
#' @importFrom graph nodes edges edgemode
#' @importFrom Matrix sparseMatrix
#' @importFrom rlang is_integerish
NULL

#' @export
as_caugi <- function(x, ...) {
  UseMethod("as_caugi")
}

#' @export
as_caugi.graphNEL <- function(x, collapse = TRUE, collapse_to = "---", ...) {
  caugi_from_graphNEL(x, collapse = collapse, collapse_to = collapse_to)
}

#' @export
as_caugi.dgCMatrix <- function(x, directed, ...) {
  caugi_from_sparse(x, directed)
}

#' @export
as_caugi.sparseMatrix <- function(x, directed, ...) {
  caugi_from_sparse(x, directed)
}

#' @export
as_caugi.matrix <- function(x, ...) {
  caugi_from_dense(x)
}

#' @export
as_caugi.amat <- function(x, ...) {
  caugi_from_amat(x)
}

#' @export
as_caugi.data.frame <- function(x, ...) {
  if (all(c("from", "to", "edge_type") %in% names(x))) {
    nodes <- tibble(name = unique(c(as.character(x$from), as.character(x$to))))
    caugi_graph(nodes, tibble(
      from = as.character(x$from),
      to = as.character(x$to),
      edge_type = as.character(x$edge_type)
    ))
  } else {
    stop("Cannot convert data.frame to caugi_graph: missing from/to/edge_type columns")
  }
}

#' @export
as_caugi.default <- function(x, ...) {
  stop("No as_caugi method for class ", paste(class(x), collapse = "/"))
}

# -------------------------------------------------------------------------
#  R/convert.R   –  patch the igraph → caugi converter
# -------------------------------------------------------------------------

#' Convert an **igraph** object to a `caugi_graph`
#'
#' @param x        An [`igraph`] object.
#' @param collapse Logical. If `TRUE` (default) *mutual* pairs
#'                 (`A→B` **and** `B→A`) that are marked as symmetric
#'                 (`edge_type` %in% c("---","<->")) are collapsed to
#'                 one row.  Set to `FALSE` to keep every edge exactly
#'                 as it appears in the igraph.
#' @param ...      Not used; kept for S3 compatibility.
#' @return A `caugi_graph`.
#' @export
as_caugi.igraph <- function(x, collapse = TRUE, ...) {
  # -- 1. Nodes --------------------------------------------------------------
  v_names <- igraph::V(x)$name %||%
    as.character(seq_len(igraph::vcount(x)))
  nodes <- tibble::tibble(name = v_names)

  # 2. Edge data frame ----------------------------------------------------
  df_e <- igraph::as_data_frame(x, what = "edges")[, c("from", "to")]

  default_code <- if (igraph::is.directed(x)) "-->" else "---"
  df_e$edge_type <- rep(default_code, nrow(df_e)) # ← length-safe

  # 2a. Edgeless igraph → edgeless caugi_graph ----------------------------
  if (nrow(df_e) == 0L) {
    return(caugi_graph(
      tibble::tibble(name = igraph::V(x)$name %||%
        as.character(seq_len(igraph::vcount(x)))),
      tibble::tibble(
        from = character(),
        to = character(),
        edge_type = character()
      )
    ))
  }

  # Validate codes
  bad <- setdiff(unique(df_e$edge_type), edge_type_levels)
  if (length(bad)) {
    stop("Unknown edge_type(s): ", paste(bad, collapse = ", "), call. = FALSE)
  }

  # -- 3. Optional collapsing ------------------------------------------------
  if (igraph::is.directed(x) && isTRUE(collapse)) {
    df_e <- collapse_directed_to_symmetric_edges(df_e)
  } else if (!igraph::is.directed(x)) {
    # ensure canonical ordering for undirected igraphs
    df_e <- df_e %>%
      dplyr::mutate(u = pmin(from, to), v = pmax(from, to)) %>%
      dplyr::transmute(from = u, to = v, edge_type)
  }

  # -- 4. Build caugi_graph --------------------------------------------------
  caugi_graph(nodes, df_e)
}



#' Helper that every operator calls
#'
#' @keywords internal
.build_edge_spec <- function(lhs, rhs, code) {
  tibble::tibble(
    from      = deparse1(lhs),
    to        = deparse1(rhs),
    edge_type = code
  ) |>
    structure(class = c("caugi_edge_spec", "tbl_df", "tbl", "data.frame"))
}

# User-facing operators
`%-->%` <- function(lhs, rhs) .build_edge_spec(substitute(lhs), substitute(rhs), "-->")
`%<->%` <- function(lhs, rhs) .build_edge_spec(substitute(lhs), substitute(rhs), "<->")
`%---%` <- function(lhs, rhs) .build_edge_spec(substitute(lhs), substitute(rhs), "---")
`%o--%` <- function(lhs, rhs) .build_edge_spec(substitute(lhs), substitute(rhs), "o--")
`%o->%` <- function(lhs, rhs) .build_edge_spec(substitute(lhs), substitute(rhs), "o->")
`%o-o%` <- function(lhs, rhs) .build_edge_spec(substitute(lhs), substitute(rhs), "o-o")

#' Create a caugi_graph (now with `%-->%` / `%<->%` / `%---%` support)
#'
#' @param ... Either the classic `nodes, edges` pair *or* any number of
#'            \code{caugi_edge_spec} objects (built by the new infix operators)
#'            and/or old two-sided formulas (`A ~ B + C`).  A leading node
#'            tibble is still allowed.
#' @return A `caugi_graph`
#' @export
caugi_graph <- function(...) {
  dots <- rlang::list2(...)

  # ── 1 ─ Classic two-tibble path (untouched) ------------------------------
  if (length(dots) == 2 &&
    is.data.frame(dots[[1]]) &&
    "name" %in% names(dots[[1]]) &&
    is.data.frame(dots[[2]]) &&
    all(c("from", "to", "edge_type") %in% names(dots[[2]]))) {
    nodes <- dots[[1]]
    edges <- dots[[2]]
  } else {
    # ── 2 ─ New flexible path  ----------------------------------------------
    ## Peel off an optional leading node tibble
    if (length(dots) && is.data.frame(dots[[1]]) &&
      "name" %in% names(dots[[1]])) {
      nodes <- dots[[1]]
      dots <- dots[-1]
    } else {
      nodes <- NULL
    }

    ## Every remaining piece must be *either* a formula *or* edge-spec object
    bad <- !vapply(
      dots, function(x) {
        inherits(x, "caugi_edge_spec") ||
          rlang::is_formula(x, lhs = TRUE)
      },
      logical(1)
    )
    if (any(bad)) {
      stop("All arguments after an optional node tibble must be\n",
        "  • a two-sided formula (A ~ B) or\n",
        "  • an edge spec produced by %-->%, %<->%, or %---%.",
        call. = FALSE
      )
    }

    # ---- a) formulas → tibble( from, to, edge_type = "-->" ) --------------
    parse_formula <- function(fml) {
      lhs <- all.vars(rlang::f_lhs(fml))
      rhs <- all.vars(rlang::f_rhs(fml))
      if (!length(lhs) || !length(rhs)) {
        stop("Formula ", deparse(fml), " must have names both sides.",
          call. = FALSE
        )
      }
      tidyr::crossing(from = lhs, to = rhs) |>
        dplyr::mutate(edge_type = "-->")
    }

    edges_formula <- dplyr::bind_rows(
      lapply(Filter(rlang::is_formula, dots), parse_formula)
    )

    # ---- b) edge-spec objects are already tibbles -------------------------
    edges_ops <- dplyr::bind_rows(
      Filter(function(x) inherits(x, "caugi_edge_spec"), dots)
    )

    edges <- dplyr::bind_rows(edges_formula, edges_ops)

    # infer nodes if absent
    if (is.null(nodes)) {
      nodes <- tibble::tibble(name = unique(c(edges$from, edges$to)))
    } else {
      missing <- setdiff(unique(c(edges$from, edges$to)), nodes$name)
      if (length(missing)) {
        stop("Edge list refers to unknown node(s): ",
          paste(missing, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  # ── 3 ─ rest of original constructor ------------------------------------
  uid <- setNames(seq_len(nrow(nodes)), nodes$name)
  from_id <- uid[edges$from]
  to_id <- uid[edges$to]
  if (anyNA(from_id) | anyNA(to_id)) {
    stop("from/to must match nodes$name")
  }

  type_codes <- as.integer(factor(edges$edge_type, levels = edge_type_levels))

  raw <- caugi_create_csr_from_csr(
    as.integer(from_id),
    as.integer(to_id),
    type_codes,
    as.integer(nrow(nodes))
  )
  caugi_graph_from_csr(nodes$name, raw)
}


#' Convert a caugi_graph to an igraph object
#'
#' @param x A caugi_graph
#' @param directed Logical; if `NULL` (the default) we auto-detect any directed edges.
#' @param ... Passed on to igraph::graph_from_data_frame()
#' @return An igraph object with an `edge_type` attribute on each edge.
#' @seealso igraph::graph_from_data_frame
#' @export
as_igraph <- function(x, ...) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' @export
as_igraph.caugi_graph <- function(x, ...) {
  # ── 1. Pull the tidy edge list -------------------------------------------------
  edges <- as_tibble(x, collapse = FALSE)

  # ── 2. Classify every edge -----------------------------------------------------
  pag_codes <- c("o->", "o--", "o-o") # not supported
  directed_codes <- c("-->") # simple arrows
  undirected_codes <- c("<->", "---") # symmetric

  if (any(edges$edge_type %in% pag_codes)) {
    stop("Conversion to igraph is not supported for PAG-type edges ",
      "(edge_type %in% {", paste(pag_codes, collapse = ", "), "}).",
      call. = FALSE
    )
  }

  edge_class <- dplyr::case_when(
    edges$edge_type %in% directed_codes ~ "directed",
    edges$edge_type %in% undirected_codes ~ "undirected",
    TRUE ~ "unknown"
  )

  # ── 3. Decide on graph directedness -------------------------------------------
  all_directed <- all(edge_class == "directed")
  all_undirected <- all(edge_class == "undirected")

  build_edges <- function(edf) {
    edf |>
      dplyr::select(from, to) |>
      dplyr::mutate(dplyr::across(everything(), as.character))
  }

  if (all_directed) {
    # (a) purely directed  -------------------------------------------------------
    ig <- igraph::graph_from_data_frame(
      build_edges(edges),
      vertices = x$nodes,
      directed = TRUE,
      ...
    )
  } else if (all_undirected) {
    # (b) purely symmetric  ------------------------------------------------------
    ig <- igraph::graph_from_data_frame(
      build_edges(edges),
      vertices = x$nodes,
      directed = FALSE,
      ...
    )
  } else {
    undirected <- edges[edge_class == "undirected", , drop = FALSE]
    directed <- edges[edge_class == "directed", , drop = FALSE]

    ## Robust duplication: no mutate(), no pronouns, no chance of chaining
    undir_dup <- undirected[, c("to", "from", "edge_type")]
    names(undir_dup)[1:2] <- c("from", "to")

    mixed_edges <- dplyr::bind_rows(directed, undirected, undir_dup)

    ig <- igraph::graph_from_data_frame(
      mixed_edges %>% dplyr::select(from, to),
      vertices = x$nodes,
      directed = TRUE,
      ...
    )
  }

  # ── 4. Preserve the original caugi edge codes ---------------------------------
  igraph::E(ig)$edge_type <- if (all_directed || all_undirected) {
    edges$edge_type
  } else {
    mixed_edges$edge_type
  }

  ig
}


#' Create a caugi_graph from a sparse dgCMatrix or sparseMatrix
#' @export
caugi_from_sparse <- function(mat, directed) {
  if (!inherits(mat, "dgCMatrix")) {
    if (inherits(mat, "sparseMatrix")) {
      mat <- methods::as(mat, "dgCMatrix")
    } else {
      stop("mat must be a sparseMatrix")
    }
  }
  ptrs <- caugi_create_csr_from_sparse(mat, directed)
  names <- rownames(mat) %||% seq_len(nrow(mat))
  caugi_graph_from_csr(as.character(names), ptrs)
}

#' Create a caugi_graph from an adjacency matrix
#'
#' @param mat A binary adjacency matrix (0/1) or a sparseMatrix
#' @export
caugi_from_adj_matrix <- function(mat, directed = TRUE) {
  if (inherits(mat, "dgCMatrix") || inherits(mat, "sparseMatrix")) {
    spr <- caugi_create_csr_from_sparse(mat, directed)
  }
  if (!rlang::is_integerish(mat)) {
    stop("mat must be an integer matrix", .call = FALSE)
  }
  if (!inherits(mat, "matrix")) {
    stop("mat must be a matrix", .call = FALSE)
  }
  mat <- as.matrix(mat)
  # check if all values are 0 or 1
  if (!all(mat %in% c(0L, 1L))) {
    stop("mat must be a binary adjacency matrix")
  }
  if (!directed) {
    mat <- mat + t(mat) # convert from directed to undirected
    mat[mat > 0] <- 6L # undirected edge code
  }
  ptrs <- caugi_create_csr_from_dense(mat)
  names <- rownames(mat) %||% seq_len(nrow(mat))
  caugi_graph_from_csr(as.character(names), ptrs)
}

#' Create a caugi_graph from an integer-coded dense matrix
#'
#' @param mat A dense matrix with integer edge codes or a pcalg::amat
#' @export
caugi_from_dense <- function(mat) {
  if (inherits(mat, "amat")) {
    caugi_from_amat(mat)
  } else {
    if (!rlang::is_integerish(mat)) {
      stop("mat must be an integer matrix", .call = FALSE)
    }
    if (!inherits(mat, "matrix")) {
      stop("mat must be a matrix", .call = FALSE)
    }
    mat <- as.matrix(mat)
    stopifnot(is.integer(mat))
    ptrs <- caugi_create_csr_from_dense(mat)
    names <- rownames(mat) %||% seq_len(nrow(mat))
    caugi_graph_from_csr(as.character(names), ptrs)
  }
}

#' Create a caugi_graph from a possibly‐mixed graphNEL
#'
#' @param g A graphNEL object
#' @param collapse Logical, whether to collapse directed edges to undirected
#' @param collapse_to The code to use for collapsed directed pairs (default "---")
#' @export
caugi_from_graphNEL <- function(g, collapse = TRUE, collapse_to = "---") {
  if (!inherits(g, "graphNEL")) {
    stop("g must be a graphNEL object", .call = FALSE)
  }
  node_names <- graph::nodes(g)
  adj <- graph::edges(g)
  edge_type <- if (graph::edgemode(g) == "undirected") "---" else "-->"
  raw <- tibble(
    from      = rep(names(adj), lengths(adj)),
    to        = unlist(adj, use.names = FALSE),
    edge_type = edge_type
  )
  df <- if (collapse) collapse_directed_to_symmetric_edges(raw, collapse_to) else raw

  uid <- setNames(seq_along(node_names), node_names)
  ord <- order(uid[df$from], uid[df$to])
  df <- df[ord, , drop = FALSE]
  caugi_graph(tibble(name = node_names), df)
}

#' Create a caugi_graph from a pcalg::amat
#' @export
caugi_from_amat <- function(mat, amat_type = NULL) {
  if (!inherits(mat, "amat")) {
    stop("mat must be a pcalg amat", .call = FALSE)
  }
  if (!rlang::is_integerish(mat)) {
    stop("mat must be an integer matrix", .call = FALSE)
  }
  if (is.null(amat_type)) {
    amat_type <- attr(mat, "type")
  } else {
    attr(mat, "type") <- amat_type
  }
  if (!amat_type %in% c("cpdag", "pag")) {
    stop("amat must have type = 'cpdag' or 'pag'", .call = FALSE)
  }

  # coerce integer matrix
  storage.mode(mat) <- "integer" # now TYPEOF(m) == INTSXP

  # call the C++ routine
  ptrs <- caugi_create_csr_from_amat(mat, amat_type)

  # build the graph
  names <- rownames(mat) %||% seq_len(nrow(mat))
  caugi_graph_from_csr(as.character(names), ptrs)
}

#' @export
print.caugi_graph <- function(x, ...) {
  print(as_tibble(x, ...))
  invisible(x)
}

#' @export
as_tibble.caugi_graph <- function(x, collapse = FALSE, collapse_to = "---", ...) {
  df <- caugi_edges_df(x)
  if (collapse) df <- collapse_directed_to_symmetric_edges(df, collapse_to)
  df
}

# ────────────────────────────── All helpers  ──────────────────────────────────

# Helper: raw edges tibble
caugi_edges_df <- function(x) {
  rp <- as.integer(x$csr$row_ptr)
  cols <- as.integer(x$csr$col_ids)
  type_codes <- as.integer(x$csr$type_codes)
  from_idx <- rep(seq_along(diff(rp)), diff(rp))
  check_edge_integer(type_codes)
  tibble(
    from      = x$nodes$name[from_idx],
    to        = x$nodes$name[cols],
    edge_type = edge_type_levels[type_codes]
  )
}

#' Reverse edges that are <-- or <-o
#'
#' Currently not used
#'
#' @keywords internal
reverse_bad_edges <- function(nodes, edges) {
  # map old reverse codes to canonical + swap
  rev_map <- c(
    "<--" = "-->",
    "<-o" = "o->",
  )
  bad <- edges$edge_type %in% names(rev_map)
  if (any(bad)) {
    swapped <- edges[bad, ]
    edges[bad, c("from", "to")] <- swapped[c("to", "from")]
    edges$edge_type[bad] <- rev_map[swapped$edge_type]
  }
  edges
}


# Internal helper: wrap a CSR list and node names into a caugi_graph
caugi_graph_from_csr <- function(node_names, ptrs) {
  stopifnot(
    is.character(node_names),
    is.list(ptrs),
    length(ptrs) == 3,
    all(c("row_ptr", "col_ids", "type_codes") %in% names(ptrs)),
    all(sapply(ptrs, is.integer))
  )
  check_edge_integer(ptrs$type_codes)
  structure(
    list(
      nodes = tibble(name = node_names),
      csr = list(
        row_ptr    = as.integer(ptrs$row_ptr),
        col_ids    = as.integer(ptrs$col_ids),
        type_codes = as.integer(ptrs$type_codes)
      )
    ),
    class = "caugi_graph"
  )
}

#' Check if edge type integer maps to edge type
#' @keywords internal
check_edge_integer <- function(x) {
  if (!rlang::is_integerish(x)) {
    stop("Edge integer must be an integer",
      .call = FALSE
    )
  }
  if (any(x < 1L | x > length(edge_type_levels))) {
    stop("Edge integer must be between 1 and ", length(edge_type_levels),
      .call = FALSE
    )
  }
  if (anyNA(x)) {
    stop("Edge integer must not be NA",
      .call = FALSE
    )
  }
  invisible(TRUE)
}


#' Collapse mutual directed edges (-->, not o->) to a single edge (--- or <->)
#'
#' @param df A tibble with columns from, to, edge_type
#' @param collapse_to The code to use for collapsed directed pairs (default "---")
#' @return A tibble with directed pairs collapsed
#' @keywords internal
collapse_directed_to_symmetric_edges <- function(df, collapse_to = "---") {
  stopifnot(is.data.frame(df), "edge_type" %in% names(df))

  # 1) identify the mutual (-->) pairs
  key <- paste(df$from, df$to, sep = "\r")
  rev_key <- paste(df$to, df$from, sep = "\r")
  is_sym <- key %in% rev_key

  # 2) asymmetric ones (keep exactly as-is)
  df_asym <- df[!is_sym, , drop = FALSE]

  # 3) symmetric ones (i→j & j→i), collapse one per unordered pair
  df_sym2 <- if (any(is_sym)) {
    df_sym <- df[is_sym, , drop = FALSE]
    # build unique unordered keys, but *preserve* the correct ordering
    pairs <- unique(paste(
      ifelse(df_sym$from < df_sym$to, df_sym$from, df_sym$to),
      ifelse(df_sym$from < df_sym$to, df_sym$to, df_sym$from),
      sep = "\r"
    ))
    # turn each key back into a single row
    do.call(rbind, lapply(pairs, function(k) {
      nodes <- strsplit(k, "\r", fixed = TRUE)[[1]]
      data.frame(
        from = nodes[1],
        to = nodes[2],
        edge_type = collapse_to,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    # no symmetric pairs
    df[FALSE, , drop = FALSE]
  }

  # 4) recombine & sort for reproducibility
  out <- rbind(df_asym, df_sym2)
  out[order(out$from, out$to), , drop = FALSE]
}

#' Collapse mutual directed PAG edges (o->, not -->) to a single o-o edge
#'
#' @param df A tibble with columns from, to, edge_type
#' @param collapse_to The code to use for collapsed directed pairs (default "---")
#' @return A tibble with directed pairs collapsed
#' @keywords internal
collapse_directed_PAG_to_symmetric_PAG <- function(df) {
  stopifnot(
    is.data.frame(df)
  )

  # split directed vs other
  df_dir <- df |> filter(edge_type == "o->")
  df_other <- df |> filter(!edge_type == "o->")

  # collapse mutual directed pairs
  df_dir_collapsed <-
    df_dir |>
    mutate(u = pmin(from, to), v = pmax(from, to)) |>
    group_by(u, v) |>
    summarize(
      edge_type = if (n() > 1) o - o else first(edge_type),
      .groups = "drop"
    ) |>
    transmute(from = u, to = v, edge_type)

  bind_rows(df_other, df_dir_collapsed) |>
    arrange(from, to)
}

#' Collapse bidirected (<->) and undirected (---) edges to single edges
#'
#' @param df A tibble with columns from, to, edge_type
#' @return A tibble with symmetric edges collapsed
#' @keywords internal
collapse_symmetric_edges <- function(df) {
  symmetric_codes <- c("<->", "o-o", "---")
  # split symmetric vs other
  df_sym <- df |> filter(edge_type %in% symmetric_codes)
  df_other <- df |> filter(!edge_type %in% symmetric_codes)

  # collapse duplicates of symmetric codes
  df_sym_collapsed <-
    df_sym |>
    mutate(u = pmin(from, to), v = pmax(from, to)) |>
    distinct(u, v, edge_type) |>
    transmute(from = u, to = v, edge_type)

  bind_rows(df_other, df_sym_collapsed) |>
    arrange(from, to)
}

# valid edge codes, in fixed order
edge_type_levels <- c("-->", "<->", "o->", "o--", "o-o", "---")
