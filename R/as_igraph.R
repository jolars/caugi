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
