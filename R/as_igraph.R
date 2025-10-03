#' Convert a caugi_graph to an igraph object
#'
#' @param x A caugi_graph
#' @param ... Passed on to igraph::graph_from_data_frame()
#' @return An igraph object with an `edge_type` attribute on each edge.
#' @seealso \code{\link[igraph:igraph-package]{the igraph package}} for full details.
#'
#' @importFrom dplyr case_when select all_of mutate across bind_rows
#' @importFrom rlang .data
#' @export
as_igraph <- function(x, ...) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' @export
as_igraph.caugi_graph <- function(x, ...) {
  # 1 ─ tidy edge list -------------------------------------------------------
  # edges <- as_tibble(x, collapse = FALSE)
  edges <- x@edges

  # 2 ─ classify edge types --------------------------------------------------
  pag_codes <- c("o->", "o--", "o-o")
  directed_codes <- "-->"
  undirected_codes <- c("<->", "---")

  if (any(edges$edge_type %in% pag_codes)) {
    stop(
      "Conversion to igraph is not supported when PAG-type edges ",
      "are present (", paste(pag_codes, collapse = ", "), ").",
      call. = FALSE
    )
  }

  edge_class <- dplyr::case_when(
    edges$edge_type %in% directed_codes ~ "directed",
    edges$edge_type %in% undirected_codes ~ "undirected",
    TRUE ~ "unknown"
  )

  # 3 ─ decide directedness ---------------------------------------------------
  all_directed <- all(edge_class == "directed")
  all_undirected <- all(edge_class == "undirected")

  build_edges <- function(edf) {
    edf |>
      dplyr::select(dplyr::all_of(c("from", "to"))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }

  if (all_directed) {
    ig <- igraph::graph_from_data_frame(
      build_edges(edges),
      vertices = x$nodes,
      directed = TRUE,
      ...
    )
  } else if (all_undirected) {
    ig <- igraph::graph_from_data_frame(
      build_edges(edges),
      vertices = x$nodes,
      directed = FALSE,
      ...
    )
  } else {
    undirected <- edges[edge_class == "undirected", , drop = FALSE]
    directed <- edges[edge_class == "directed", , drop = FALSE]

    # duplicate undirected edges in reverse orientation
    undir_dup <- undirected[, c("to", "from", "edge_type")]
    names(undir_dup)[1:2] <- c("from", "to")

    mixed_edges <- dplyr::bind_rows(directed, undirected, undir_dup)

    ig <- igraph::graph_from_data_frame(
      mixed_edges |>
        dplyr::select(dplyr::all_of(c("from", "to"))),
      vertices = x$nodes,
      directed = TRUE,
      ...
    )
  }

  # 4 ─ preserve original caugi edge codes -----------------------------------
  igraph::E(ig)$edge_type <-
    if (all_directed || all_undirected) edges$edge_type else mixed_edges$edge_type

  ig
}
