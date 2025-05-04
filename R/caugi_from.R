#' @rdname as_caugi.igraph
#' @export
caugi_from_igraph <- function(x, collapse = FALSE, collapse_to = "---") {
  # -- 1. Nodes --------------------------------------------------------------
  v_names <- igraph::V(x)$name %||%
    as.character(seq_len(igraph::vcount(x)))
  nodes <- tibble::tibble(name = v_names)

  # 2. Edge data frame ----------------------------------------------------
  df_e <- igraph::as_data_frame(x, what = "edges")[, c("from", "to")]

  default_code <- if (igraph::is_directed(x)) "-->" else "---"
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
  if (igraph::is_directed(x) && isTRUE(collapse)) {
    df_e <- collapse_directed_to_symmetric_edges(df_e, collapse_to)
  } else if (!igraph::is_directed(x)) {
    # ensure canonical ordering for undirected igraphs
    df_e <- df_e %>%
      dplyr::mutate(u = pmin(from, to), v = pmax(from, to)) %>%
      dplyr::transmute(from = u, to = v, edge_type)
  }

  # -- 4. Build caugi_graph --------------------------------------------------
  caugi_graph(nodes, df_e)
}


#' @rdname as_caugi.sparseMatrix
#' @export
caugi_from_sparse <- function(x, directed) {
  if (!inherits(x, "dgCMatrix")) {
    if (inherits(x, "sparseMatrix")) {
      x <- methods::as(x, "dgCMatrix")
    } else {
      stop("x must be a sparseMatrix")
    }
  }
  ptrs <- caugi_create_csr_from_sparse(x, directed)
  names <- rownames(x) %||% seq_len(nrow(x))
  caugi_graph_from_csr(as.character(names), ptrs)
}

#' @title Create a caugi_graph from an adjacency matrix
#'
#' @description Create a `caugi_graph` from a binary adjacency matrix. Can be
#' both a `sparseMatrix`, `dgCMatrix`, or a dense matrix.
#'
#' @param x A binary adjacency matrix (0/1) or a sparseMatrix
#' @export
caugi_from_adj_matrix <- function(x, directed = TRUE) {
  if (inherits(x, "dgCMatrix") || inherits(x, "sparseMatrix")) {
    spr <- caugi_create_csr_from_sparse(x, directed)
  }
  if (!rlang::is_integerish(x)) {
    stop("x must be an integer matrix", .call = FALSE)
  }
  if (!inherits(x, "matrix")) {
    stop("x must be a matrix", .call = FALSE)
  }
  x <- as.matrix(x)
  # check if all values are 0 or 1
  if (!all(x %in% c(0L, 1L))) {
    stop("x must be a binary adjacency matrix")
  }
  if (!directed) {
    x <- x + t(x) # convert from directed to undirected
    x[x > 0] <- 6L # undirected edge code
  }
  ptrs <- caugi_create_csr_from_dense(x)
  names <- rownames(x) %||% seq_len(nrow(x))
  caugi_graph_from_csr(as.character(names), ptrs)
}

#' @rdname as_caugi.matrix
#' @export
caugi_from_dense <- function(x) {
  if (inherits(x, "amat")) {
    caugi_from_amat(x)
  } else {
    if (!rlang::is_integerish(x)) {
      stop("x must be an integer matrix", .call = FALSE)
    }
    if (!inherits(x, "matrix")) {
      stop("x must be a matrix", .call = FALSE)
    }
    x <- as.matrix(x)
    stopifnot(is.integer(x))
    ptrs <- caugi_create_csr_from_dense(x)
    names <- rownames(x) %||% seq_len(nrow(x))
    caugi_graph_from_csr(as.character(names), ptrs)
  }
}

#' @rdname as_caugi.graphNEL
#' @export
caugi_from_graphNEL <- function(x, collapse = FALSE, collapse_to = "---") {
  if (!inherits(x, "graphNEL")) {
    stop("x must be a graphNEL object", .call = FALSE)
  }
  node_names <- graph::nodes(x)
  adj <- graph::edges(x)
  edge_type <- if (graph::edgemode(x) == "undirected") "---" else "-->"
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

#' @rdname as_caugi.amat
#' @export
caugi_from_amat <- function(x, amat_type = NULL) {
  if (!inherits(x, "amat")) {
    stop("x must be a pcalg amat", .call = FALSE)
  }
  if (!rlang::is_integerish(x)) {
    stop("x must be an integer matrix", .call = FALSE)
  }
  if (is.null(amat_type)) {
    amat_type <- attr(x, "type")
  } else {
    attr(x, "type") <- amat_type
  }
  if (!amat_type %in% c("cpdag", "pag")) {
    stop("amat must have type = 'cpdag' or 'pag'", .call = FALSE)
  }

  # coerce integer matrix
  storage.mode(x) <- "integer" # now TYPEOF(m) == INTSXP

  # call the C++ routine
  ptrs <- caugi_create_csr_from_amat(x, amat_type)

  # build the graph
  names <- rownames(x) %||% seq_len(nrow(x))
  caugi_graph_from_csr(as.character(names), ptrs)
}
