#' @title Convert to a `caugi_graph`
#'
#' @description Convert an object to a `caugi_graph`. The object can be a
#' `graphNEL`, `sparseMatrix`, `dgCMatrix`, `matrix`, `pcalg`s `amat`,
#' or a data frame with `from`, `to`, and `edge_type` columns.
#'
#' @param x An object to convert to a `caugi_graph`.
#' @param ... Additional arguments passed to specific methods.
#' @importFrom stats setNames
#' @export
as_caugi <- function(x, ...) {
  UseMethod("as_caugi")
}

#' @title Create a caugi_graph from a possibly‐mixed graphNEL
#'
#' @param x A graphNEL object
#' @param collapse Logical, whether to collapse directed edges to undirected
#' @param collapse_to The code to use for collapsed directed pairs (default "---")
#' @param ... Additional arguments (will not be passed to anything).
#' @export
as_caugi.graphNEL <- function(x, collapse = FALSE, collapse_to = "---", ...) {
  NULL
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

#' Convert an **igraph** object to a `caugi_graph`
#'
#' @param x        An `igraph` object.
#' @param collapse Logical. If `TRUE` (default) *mutual* pairs
#'                 (`A-->B` **and** `B-->A`) that are marked as symmetric
#'                 (`edge_type` %in% c("---","<->")) are collapsed to
#'                 one row.  Set to `FALSE` to keep every edge exactly
#'                 as it appears in the igraph.
#' @param collapse_to The code to use for collapsed directed pairs (default "---")
#' @param ...      Not used; kept for S3 compatibility.
#' @seealso \code{\link[igraph:igraph-package]{the igraph package}} for details.
#' @return A `caugi_graph`.
#' @export
as_caugi.igraph <- function(x, collapse = FALSE, collapse_to = "---", ...) {
  NULL
}
