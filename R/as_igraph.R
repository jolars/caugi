#' @title Convert a caugi_graph to an igraph object
#'
#' @param x A `caugi_graph` object.
#' @param ... Additional arguments passed to `igraph::graph_from_data_frame()`.
#'
#' @returns An `igraph` object representing the same graph structure.
#'
#' @family conversion
#' @concept conversion
#'
#' @export
as_igraph <- function(x, ...) {
  is_caugi(x, throw_error = TRUE)

  if (!(x@graph_class %in% c("DAG", "PDAG", "UG", "UNKNOWN"))) {
    stop("caugi graphs of class '", x@graph_class, "' cannot be converted to ",
      "igraph objects.",
      call. = FALSE
    )
  }

  et <- edge_types(x)

  if (x@graph_class == "UG") {
    directed <- FALSE
  } else if (length(et) == 0L) {
    directed <- FALSE
  } else if (all(et %in% c("---", "<->"))) { # treat both as undirected
    directed <- FALSE
  } else {
    directed <- TRUE
  }

  # empty graph
  if (nrow(edges(x)) == 0L) {
    return(igraph::graph_from_data_frame(
      tibble::tibble(
        from = character(0),
        to = character(0)
      ),
      vertices = nodes(x),
      directed = directed, ...
    ))
  }

  # edge validity for Unknown
  if (x@graph_class == "UNKNOWN") {
    if (any(!(et %in% c("-->", "<->", "---")))) {
      stop("Conversion to igraph is only supported for 'Unknown' caugi graphs ",
        " with '-->', '<->', or '---' edges.",
        call. = FALSE
      )
    }
  }

  e <- edges(x)

  if (!directed) {
    # all undirected
    return(igraph::graph_from_data_frame(
      tibble::tibble(
        from = pmin(e$from, e$to),
        to   = pmax(e$from, e$to)
      ),
      vertices = nodes(x),
      directed = FALSE, ...
    ))
  } else if (all(et %in% "-->")) {
    # all directed
    return(igraph::graph_from_data_frame(
      tibble::tibble(from = e$from, to = e$to),
      vertices = nodes(x), directed = TRUE, ...
    ))
  } else {
    # mixed: keep directed as-is, duplicate undirected as bidirected
    dir_df <- e[e$edge == "-->", c("from", "to"), drop = FALSE]

    undir <- e[e$edge %in% c("<->", "---"), c("from", "to"), drop = FALSE]
    u_from <- pmin(undir$from, undir$to)
    u_to <- pmax(undir$from, undir$to)
    undir_fwd <- tibble::tibble(from = u_from, to = u_to)
    undir_rev <- tibble::tibble(from = u_to, to = u_from)
    bi_df <- rbind(undir_fwd, undir_rev)

    all_df <- rbind(
      tibble::tibble(
        from = dir_df$from,
        to = dir_df$to
      ),
      bi_df
    )

    return(igraph::graph_from_data_frame(
      all_df,
      vertices = nodes(x), directed = TRUE, ...
    ))
  }
}
