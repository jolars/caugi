# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Queries ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# ────────────────────────────────── Checks ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Is it a `caugi` graph?
#'
#' @description Checks if the given object is a `caugi_graph`.
#'
#' @param x An object to check.
#' @param throw_error Logical; if `TRUE`, throws an error if `x` is not a
#' `caugi_graph`.
#'
#' @returns A logical value indicating whether the object is a `caugi_graph`.
#'
#' @family queries
#' @concept queries
#'
#' @export
is_caugi <- function(x, throw_error = FALSE) {
  it_is <- inherits(x, caugi_graph)

  if (!it_is && throw_error) {
    stop("Input must be a caugi_graph", call. = FALSE)
  }
  it_is
}

#' @title Is the `caugi` graph empty?
#'
#' @description Checks if the given `caugi` graph is empty (has no nodes).
#'
#' @param cg A `caugi_graph` object.
#'
#' @returns A logical value indicating whether the graph is empty.
#'
#' @family queries
#' @concept queries
#'
#' @export
is_empty_caugi <- function(cg) {
  is_caugi(cg, throw_error = TRUE)
  nrow(cg@nodes) == 0L
}

#' @title Same nodes?
#'
#' @description Check if two `caugi_graph` objects have the same nodes.
#'
#' @param cg1 A `caugi_graph` object.
#' @param cg2 A `caugi_graph` object.
#' @param throw_error Logical; if `TRUE`, throws an error if the graphs do not
#' have the same nodes.
#'
#' @returns A logical indicating if the two graphs have the same nodes.
#'
#' @family queries
#' @concept queries
#'
#' @export
same_nodes <- function(cg1, cg2, throw_error = FALSE) {
  is_caugi(cg1, throw_error)
  is_caugi(cg2, throw_error)
  if (!setequal(cg1@nodes$name, cg2@nodes$name)) {
    differing_nodes <- setdiff(
      union(cg1@nodes$name, cg2@nodes$name),
      intersect(cg1@nodes$name, cg2@nodes$name)
    )

    stop(
      "Graphs must have the same nodes.\n",
      "Differing nodes are: [", paste(differing_nodes, collapse = ", "),
      "]."
    )
  }
  TRUE
}

#' @title Is the `caugi` acyclic?
#'
#' @description Checks if the given `caugi` graph is acyclic.
#'
#' @param cg A `caugi_graph` object.
#' @param force_check Logical; if `TRUE`, the function will test if the graph is
#' acyclic, if `FALSE` (default), it will look at the graph class.
#'
#' @details
#' __Lazy building__
#'
#' If the graph is not built, the graph will be built before making the query.
#'
#' __Check logic__
#'
#' By the construction of the `caugi_graph`, if the graph class is "DAG" or
#' "PDAG", the graph should be guaranteed to be acyclic. If the graph class is
#' "UNKNOWN", the graph may or may not be acyclic. If `force_check = TRUE`, the
#' function will check if the graph is acyclic using the Rust backend, no matter
#' the class. If the graph class is not inherently acyclic, `is_acyclic` will
#' run the check.
#'
#' __Note__
#'
#' We do not think that it is possible to introduce a cycle in a `caugi_graph`
#' on acyclic classes without the package throwing errors. We might be wrong,
#' though, so `force_check` is e a feature that is there for peace of mind.
#'
#'
#' @returns A logical value indicating whether the graph is acyclic.
#'
#' @family queries
#' @concept queries
#'
#' @export
is_acyclic <- function(cg, force_check = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  if (force_check) {
    is_it <- is_acyclic_ptr(cg@ptr)
  } else if (identical(cg@graph_class, "DAG") ||
    identical(cg@graph_class, "PDAG")) {
    is_it <- TRUE
  } else {
    is_it <- is_acyclic_ptr(cg@ptr)
  }
  is_it
}

#' @title Is the `caugi` graph a DAG?
#'
#' @description Checks if the given `caugi` graph is a
#' Directed Acyclic Graph (DAG).
#'
#' @param cg A `caugi_graph` object.
#' @param force_check Logical; if `TRUE`, the function will test if the graph is
#' a DAG, if `FALSE` (default), it will look at the graph class and match
#' it, if possible.
#'
#' @returns A logical value indicating whether the graph is a DAG.
#'
#' @family queries
#' @concept queries
#'
#' @export
is_dag <- function(cg, force_check = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  if (identical(cg@graph_class, "DAG") && !force_check) {
    is_it <- TRUE
  } else {
    # if we can't be sure from the class, we check
    is_it <- is_dag_type_ptr(cg@ptr)
  }
  is_it
}

#' @title Is the `caugi` graph a PDAG?
#'
#' @description Checks if the given `caugi` graph is a
#' Partially Directed Acyclic Graph (PDAG).
#'
#' @param cg A `caugi_graph` object.
#' @param force_check Logical; if `TRUE`, the function will test if the graph is
#' a PDAG, if `FALSE` (default), it will look at the graph class and match
#' it, if possible.
#'
#' @returns A logical value indicating whether the graph is a PDAG.
#'
#' @family queries
#' @concept queries
#'
#' @export
is_pdag <- function(cg, force_check = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  if (identical(cg@graph_class, "PDAG") && !force_check) {
    is_it <- TRUE
  } else {
    # if we can't be sure from the class, we check
    is_it <- is_pdag_type_ptr(cg@ptr)
  }
  is_it
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── Nodes and edges ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Get nodes or edges of a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#'
#' @returns A tibble with a `name` column.
#'
#' @rdname nodes
#' @family queries
#' @concept queries
#'
#' @export
nodes <- function(cg) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  cg@nodes
}

#' @rdname nodes
#' @export
vertices <- nodes

#' @rdname nodes
#' @export
V <- nodes # igraph notation

#' @title Get edges of a `caugi_graph`.
#'
#' @param cg A `caugi_graph` object.
#'
#' @rdname edges
#' @concept queries
#'
#' @family queries
#' @concept queries
#'
#' @export
edges <- function(cg) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  cg@edges
}

#' @rdname edges
#' @export
E <- edges # igraph notation

#' @title Get the edge types of a `caugi_graph`.
#'
#' @param cg A `caugi_graph` object.
#'
#' @returns A character vector of edge types.
#'
#' @family queries
#' @concept queries
#'
#' @export
edge_types <- function(cg) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  unique(cg@edges$edge)
}
# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── Relations ───────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Get parents of nodes in a `caugi_graph`
#'
#' @description
#' Get parents of node in a graph. Note that not both nodes and index can be
#' given.
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names, a vector of unquoted
#' node names, or an expression combining these with `+` and `c()`.
#' @param index A vector of node indexes.
#'
#' @returns Either a character vector of node names (if a single node is
#' requested) or a list of character vectors (if multiple nodes are requested).
#'
#' @family queries
#' @concept queries
#'
#' @export
parents <- function(cg, nodes = NULL, index = NULL) {
  nodes_supplied <- !missing(nodes)
  index_supplied <- !missing(index) && !is.null(index)
  if (nodes_supplied && index_supplied) {
    stop("Supply either `nodes` or `index`, not both.", call. = FALSE)
  }
  if (!cg@built) cg <- build(cg)
  if (index_supplied) {
    return(.getter_output(
      cg,
      parents_of_ptr(cg@ptr, as.integer(index - 1L)),
      cg@nodes$name[index]
    ))
  }
  if (!nodes_supplied) {
    stop("Supply one of `nodes` or `index`.", call. = FALSE)
  }
  if (!is.character(nodes)) {
    stop("`nodes` must be a character vector of node names.", call. = FALSE)
  }

  index <- cg@name_index_map$mget(nodes,
    missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(nodes, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  )

  .getter_output(cg, parents_of_ptr(cg@ptr, as.integer(index)), nodes)
}

#' @title Get children of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names, a vector of unquoted
#' node names, or an expression combining these with `+` and `c()`.
#' @param index A vector of node indexes.
#'
#' @returns Either a character vector of node names (if a single node is
#' requested) or a list of character vectors (if multiple nodes are requested).
#'
#' @family queries
#' @concept queries
#'
#' @export
children <- function(cg, nodes = NULL, index = NULL) {
  nodes_supplied <- !missing(nodes)
  index_supplied <- !missing(index) && !is.null(index)
  if (nodes_supplied && index_supplied) {
    stop("Supply either `nodes` or `index`, not both.", call. = FALSE)
  }
  if (!cg@built) cg <- build(cg)
  if (index_supplied) {
    return(.getter_output(
      cg,
      children_of_ptr(cg@ptr, as.integer(index - 1L)),
      cg@nodes$name[index]
    ))
  }
  if (!nodes_supplied) {
    stop("Supply one of `nodes` or `index`.", call. = FALSE)
  }
  if (!is.character(nodes)) {
    stop("`nodes` must be a character vector of node names.", call. = FALSE)
  }

  index <- cg@name_index_map$mget(nodes,
    missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(nodes, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  )

  .getter_output(cg, children_of_ptr(cg@ptr, as.integer(index)), nodes)
}

#' @title Get neighbors of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names, a vector of unquoted
#' node names, or an expression combining these with `+` and `c()`.
#' @param index A vector of node indexes.
#'
#' @returns Either a character vector of node names (if a single node is
#' requested) or a list of character vectors (if multiple nodes are requested).
#'
#' @family queries
#' @concept queries
#'
#' @export
neighbors <- function(cg, nodes = NULL, index = NULL) {
  nodes_supplied <- !missing(nodes)
  index_supplied <- !missing(index) && !is.null(index)
  if (nodes_supplied && index_supplied) {
    stop("Supply either `nodes` or `index`, not both.", call. = FALSE)
  }
  if (!cg@built) cg <- build(cg)
  if (index_supplied) {
    return(.getter_output(
      cg,
      neighbors_of_ptr(cg@ptr, as.integer(index - 1L)),
      cg@nodes$name[index]
    ))
  }
  if (!nodes_supplied) {
    stop("Supply one of `nodes` or `index`.", call. = FALSE)
  }
  if (!is.character(nodes)) {
    stop("`nodes` must be a character vector of node names.", call. = FALSE)
  }

  index <- cg@name_index_map$mget(nodes,
    missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(nodes, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  )

  .getter_output(cg, neighbors_of_ptr(cg@ptr, as.integer(index)), nodes)
}

#' @rdname neighbors
#' @export
neighbours <- neighbors

#' @title Get ancestors of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names, a vector of unquoted
#' node names, or an expression combining these with `+` and `c()`.
#' @param index A vector of node indexes.
#'
#' @returns Either a character vector of node names (if a single node is
#' requested) or a list of character vectors (if multiple nodes are requested).
#'
#' @family queries
#' @concept queries
#'
#' @export
ancestors <- function(cg, nodes = NULL, index = NULL) {
  nodes_supplied <- !missing(nodes)
  index_supplied <- !missing(index) && !is.null(index)
  if (nodes_supplied && index_supplied) {
    stop("Supply either `nodes` or `index`, not both.", call. = FALSE)
  }
  if (!cg@built) cg <- build(cg)
  if (index_supplied) {
    return(.getter_output(
      cg,
      ancestors_of_ptr(cg@ptr, as.integer(index - 1L)),
      cg@nodes$name[index]
    ))
  }
  if (!nodes_supplied) {
    stop("Supply one of `nodes` or `index`.", call. = FALSE)
  }
  if (!is.character(nodes)) {
    stop("`nodes` must be a character vector of node names.", call. = FALSE)
  }

  index <- cg@name_index_map$mget(nodes,
    missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(nodes, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  )

  .getter_output(cg, ancestors_of_ptr(cg@ptr, as.integer(index)), nodes)
}

#' @title Get descendants of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names, a vector of unquoted
#' node names, or an expression combining these with `+` and `c()`.
#' @param index A vector of node indexes.
#'
#' @returns Either a character vector of node names (if a single node is
#' requested) or a list of character vectors (if multiple nodes are requested).
#'
#' @family queries
#' @concept queries
#'
#' @export
descendants <- function(cg, nodes = NULL, index = NULL) {
  nodes_supplied <- !missing(nodes)
  index_supplied <- !missing(index) && !is.null(index)
  if (nodes_supplied && index_supplied) {
    stop("Supply either `nodes` or `index`, not both.", call. = FALSE)
  }
  if (!cg@built) cg <- build(cg)
  if (index_supplied) {
    return(.getter_output(
      cg,
      descendants_of_ptr(cg@ptr, as.integer(index - 1L)),
      cg@nodes$name[index]
    ))
  }
  if (!nodes_supplied) {
    stop("Supply one of `nodes` or `index`.", call. = FALSE)
  }
  if (!is.character(nodes)) {
    stop("`nodes` must be a character vector of node names.", call. = FALSE)
  }

  index <- cg@name_index_map$mget(nodes,
    missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(nodes, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  )

  .getter_output(cg, descendants_of_ptr(cg@ptr, as.integer(index)), nodes)
}

#' @title Get Markov blanket of nodes in a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names, a vector of unquoted
#' node names, or an expression combining these with `+` and `c()`.
#' @param index A vector of node indexes.
#'
#' @returns Either a character vector of node names (if a single node is
#' requested) or a list of character vectors (if multiple nodes are requested).
#'
#' @family queries
#' @concept queries
#'
#' @export
markov_blanket <- function(cg, nodes = NULL, index = NULL) {
  nodes_supplied <- !missing(nodes)
  index_supplied <- !missing(index) && !is.null(index)
  if (nodes_supplied && index_supplied) {
    stop("Supply either `nodes` or `index`, not both.", call. = FALSE)
  }
  if (!cg@built) cg <- build(cg)
  if (index_supplied) {
    return(.getter_output(
      cg,
      markov_blanket_of_ptr(cg@ptr, as.integer(index - 1L)),
      cg@nodes$name[index]
    ))
  }
  if (!nodes_supplied) {
    stop("Supply one of `nodes` or `index`.", call. = FALSE)
  }
  if (!is.character(nodes)) {
    stop("`nodes` must be a character vector of node names.", call. = FALSE)
  }

  index <- cg@name_index_map$mget(nodes,
    missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(nodes, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  )

  .getter_output(cg, markov_blanket_of_ptr(cg@ptr, as.integer(index)), nodes)
}

#' @title Get all exogenous nodes in a `caugi_graph`
#'
#' @description Get all exogenous nodes (nodes with no parents) in a
#' `caugi_graph`.
#'
#' @param cg A `caugi_graph` object.
#' @param undirected_as_parents Logical; if `TRUE`, undirected edges are treated
#' as (possible) parents, if `FALSE` (default), undirected edges are ignored.
#'
#' @returns Either a character vector of node names (if a single node is
#' requested) or a list of character vectors (if multiple nodes are requested).
#'
#' @family queries
#' @concept queries
#'
#' @export
exogenous <- function(cg, undirected_as_parents = FALSE) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  idx0 <- exogenous_nodes_of_ptr(cg@ptr, undirected_as_parents)
  cg@nodes$name[idx0 + 1L]
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Subgraph ───────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────


#' @title Get the induced subgraph
#'
#' @param cg A `caugi_graph` object.
#' @param nodes A vector of node names, a vector of unquoted
#' node names, or an expression combining these with `+` and `c()`.
#' @param index A vector of node indexes.
#'
#' @returns A new `caugi_graph` that is a subgraph of the selected nodes.
#'
#' @family queries
#' @concept queries
#'
#' @export
subgraph <- function(cg, nodes = NULL, index = NULL) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)

  nodes_supplied <- !missing(nodes) && !is.null(nodes)
  index_supplied <- !missing(index) && !is.null(index)
  if (nodes_supplied && index_supplied) {
    stop("Supply either `nodes` or `index`, not both.", call. = FALSE)
  }
  if (!nodes_supplied && !index_supplied) {
    stop("Supply one of `nodes` or `index`.", call. = FALSE)
  }

  if (index_supplied) {
    idx1 <- as.integer(index)
    if (any(idx1 < 1L) || any(idx1 > nrow(cg@nodes))) {
      stop("`index` out of range (1..n).", call. = FALSE)
    }
    keep_names <- cg@nodes$name[idx1]
  } else {
    if (!is.character(nodes)) {
      stop("`nodes` must be a character vector.", call. = FALSE)
    }
    missing <- setdiff(nodes, cg@nodes$name)
    if (length(missing)) {
      stop("Unknown node(s): ", paste(missing, collapse = ", "), call. = FALSE)
    }
    keep_names <- nodes
  }

  if (any(duplicated(keep_names))) {
    dups <- unique(keep_names[duplicated(keep_names)])
    stop("`nodes`/`index` contains duplicates: ",
      paste(dups, collapse = ", "),
      call. = FALSE
    )
  }

  # filter edges to the kept nodes; keep constructor’s sort
  keep_set <- fastmap::fastmap()
  for (nm in keep_names) keep_set$set(nm, TRUE)
  edges_sub <- cg@edges |>
    dplyr::filter(keep_set$has(.data$from) & keep_set$has(.data$to)) |>
    dplyr::arrange(.data$from, .data$to, .data$edge)

  # rebuild via constructor: declared nodes contain all edge nodes
  caugi_graph(
    from   = edges_sub$from,
    edge   = edges_sub$edge,
    to     = edges_sub$to,
    nodes  = keep_names,
    simple = cg@simple,
    build  = TRUE,
    class  = cg@graph_class
  )
}



# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── Relations helpers ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Output object of getter queries
#'
#' @description Helper to format the output of getter queries.
#'
#' @param cg A `caugi_graph` object.
#' @param idx0 A vector of zero-based node indices.
#' @param nodes A vector of node names.
#'
#' @returns A list of character vectors, each a set of node names.
#' If only one node is requested, returns a character vector.
#'
#' @keywords internal
.getter_output <- function(cg, idx0, nodes) {
  nm <- cg@nodes$name
  to_names <- function(ix0) {
    if (length(ix0) == 0L) {
      return(NULL)
    }
    nm[ix0 + 1L]
  }

  # faster check than doing is.null and length == 1, since length(NULL) == 0
  if (length(nodes) <= 1L && length(idx0) == 1L) {
    ix <- idx0[[1L]]
    return(to_names(ix))
  }

  out <- lapply(idx0, to_names)
  names(out) <- nodes
  out
}
