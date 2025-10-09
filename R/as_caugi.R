#' @title Convert to a `caugi_graph`
#'
#' @description Convert an object to a `caugi_graph`. The object can be a
#' `graphNEL`, `sparseMatrix`, `dgCMatrix`, `matrix`, `pcalg`'s `amat`,
#' or a data frame with `from`, `to`, and `edge_type` columns.
#'
#' @param x An object to convert to a `caugi_graph`.
#' @param class "auto", "DAG", "PDAG", or "Unknown".
#'   "auto": DAG if directed and acyclic, PDAG if undirected or mixed with
#'   acyclic directed part, otherwise "Unknown".
#' @param simple logical. If `TRUE` (default) the graph will be simple
#' (no multiple edges or self-loops).
#' @param build logical. If `TRUE` (default) build the graph now, otherwise
#' build lazily on first query or when using [build()].
#' @param collapse logical. If `TRUE` collapse mutual directed edges to
#' undirected edges. Default is `FALSE`.
#' @param collapse_to Character string to use as the edge glyph when collapsing.
#' Should be a registered symmetrical edge glyph. Default is `"---"`.
#' @param ... Additional arguments passed to specific methods.
#'
#' @export
as_caugi <- S7::new_generic("as_caugi", "x")


#' @name as_caugi
#' @export
if (requireNamespace("igraph", quietly = TRUE)) {
  S7::method(as_caugi, S7::new_S3_class("igraph")) <- function(x,
                                                               class = c(
                                                                 "auto",
                                                                 "DAG",
                                                                 "PDAG",
                                                                 "Unknown"
                                                               ),
                                                               simple = igraph::is_simple(x),
                                                               build = TRUE,
                                                               collapse = FALSE,
                                                               collapse_to = "---") {
    class <- match.arg(class)

    n_edges <- igraph::ecount(x)
    directed <- igraph::is_directed(x)

    pick_class <- switch(class,
      auto = if (igraph::is_dag(x)) {
        "DAG"
      } else if (!directed && is_acyclic(x)) {
        "PDAG"
      } else {
        "Unknown"
      },
      DAG = "DAG",
      PDAG = "PDAG",
      Unknown = "Unknown"
    )

    if (n_edges == 0L) {
      return(caugi_graph(
        from = character(), edge = character(), to = character(),
        simple = isTRUE(simple), build = isTRUE(build), class = pick_class
      ))
    }

    e <- igraph::ends(x, igraph::E(x), names = TRUE)
    glyph <- if (directed) "-->" else "---"

    from <- e[, 1]
    to <- e[, 2]
    edge <- rep_len(glyph, length.out = nrow(e))

    # collapse symmetrical edges
    if (collapse && directed) {
      # check if collapse_to is registered and symmetric (throws error if not)
      is_edge_symmetric(collapse_to)

      # pairwise canonical order
      canon_from <- pmin(from, to)
      canon_to <- pmax(from, to)
      key <- interaction(canon_from, canon_to, drop = TRUE)

      # reverse exists for this row
      pair_key <- interaction(from, to, drop = TRUE)
      rev_key <- interaction(to, from, drop = TRUE)
      has_rev <- (from != to) & match(pair_key, rev_key, nomatch = 0L) > 0L

      # keep asymmetric rows, plus the first canonical rep of each symmetric group
      keep_rep <- !duplicated(key) & (from == canon_from)
      keep <- !has_rev | keep_rep

      # write collapsed rows
      from <- ifelse(has_rev & keep, canon_from, from)[keep]
      to <- ifelse(has_rev & keep, canon_to, to)[keep]
      edge <- ifelse(has_rev & keep, collapse_to, edge)[keep]
    }
    caugi_graph(
      from = from,
      edge = edge,
      to = to,
      simple = isTRUE(simple),
      build = isTRUE(build),
      class = pick_class
    )
  }
}


#' @name as_caugi
#' @export
if (requireNamespace("graph", quietly = TRUE)) {
  S7::method(as_caugi, methods::getClass("graphNEL")) <- function(x, collapse = FALSE, collapse_to = "---", ...) {
    NULL
  }
}

#' @rdname as_caugi.sparseMatrix
#' @export
as_caugi.dgCMatrix <- function(x, directed = TRUE, ...) {
  NULL
}

#' @title Create a caugi_graph from a sparse `dgCMatrix` or `sparseMatrix`
#'
#' @param x A sparse matrix of class `dgCMatrix` or `sparseMatrix`.
#' @param directed Logical; if `TRUE` (default) the graph is directed.
#' @param ... Additional arguments (will not be passed to anything).
#' @export
as_caugi.sparseMatrix <- function(x, directed = TRUE, ...) {
  NULL
}

#' @title Create a caugi_graph from an integer-coded dense matrix
#'
#' @description Create a `caugi_graph` from a dense matrix with integer edge codes.
#' The edge codes are:
#' * 0: no edge
#' * 1: directed edge (from --> to)
#' * 2: bidirectional edge (from <-> to)
#' * 3: PAG directed edge (from o-> to)
#' * 4: PAG undirected edge (from o-- to)
#' * 5: PAG bidirected edge (from o-o to)
#' * 6: undirected edge (from --- to)
#'
#' @param x A dense matrix with integer edge codes
#' @param ... Additional arguments (will not be passed to anything).
#' @export
as_caugi.matrix <- function(x, ...) {
  NULL
}

#' @title Convert a pcalg amat to a `caugi_graph`
#'
#' @description Convert a pcalg adjacency matrix to a `caugi_graph`.
#' The matrix must be a square matrix with integer edge codes.
#' See pcalg documentation on amat for more details on the edge codes in amat.
#'
#' @param x A pcalg adjacency matrix (amat).
#' @param amat_type The type of the adjacency matrix, `"cpdag"` or `"pag`.
#' @param ... Additional arguments (will not be passed to anything).
#' @export
as_caugi.amat <- function(x, amat_type = NULL, ...) {
  NULL
}

#' @title Convert a data frame to a `caugi_graph`
#'
#' @description Should be a data frame with columns `from`, `to`, and `edge_type`.
#'
#' @param x A data frame with columns `from`, `to`, and `edge_type`.
#' @param ... Additional arguments (will not be passed to anything).
#' @export
as_caugi.data.frame <- function(x, ...) {
  NULL
}

#' @title Default method for `as_caugi`
#'
#' @description Default method for `as_caugi` that throws an error.
#'
#' @param x An object to convert to a `caugi_graph`.
#' @param ... Additional arguments (will not be passed to anything).
#' @export
as_caugi.default <- function(x, ...) {
  stop("No as_caugi method for class ", paste(class(x), collapse = "/"))
}
