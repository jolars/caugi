# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────── d-separation ─────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Are X and Y d-separated given Z?
#'
#' @description Checks whether every node in `X` is d-separated from every node
#' in `Y` given `Z` in a DAG.
#'
#' @param cg A `caugi_graph` object.
#' @param X,Y,Z Node selectors: character vector of names, unquoted expression
#'   (supports `+` and `c()`), or `NULL`. Use `*_index` to pass 1-based indices.
#'   If `Z` is `NULL` or missing, no nodes are conditioned on.
#' @param X_index,Y_index,Z_index Optional numeric 1-based indices (exclusive
#'   with `X`,`Y`,`Z` respectively).
#'
#' @returns Logical scalar.
#'
#' @export
d_separated <- function(cg,
                        X = NULL, Y = NULL, Z = NULL,
                        X_index = NULL, Y_index = NULL, Z_index = NULL) {
  is_caugi(cg, TRUE)
  cg <- build(cg)

  X_idx0 <- .resolve_idx0_get(cg@name_index_map, X, X_index)
  Y_idx0 <- .resolve_idx0_get(cg@name_index_map, Y, Y_index)
  Z_idx0 <- .resolve_idx0_mget(cg@name_index_map, Z, Z_index)

  d_separated_ptr(cg@ptr, X_idx0, Y_idx0, Z_idx0)
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── Adjustment sets ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Compute an adjustment set
#'
#' @description Computes an adjustment set for `X -> Y` in a DAG.
#' @details
#' Types supported:
#' - `"parents"`: \eqn{\bigcup \mathrm{Pa}(X)} minus \eqn{X \cup Y}
#' - `"backdoor"`: Pearl backdoor formula
#' - `"optimal"`: O-set (only for single `x` and single `y`)
#'
#' @param cg A `caugi_graph` object.
#' @param X,Y Node names.
#' @param X_index,Y_index Optional numeric 1-based indices.
#' @param type One of `"parents"`, `"backdoor"`, `"optimal"`.
#'
#' @returns A tibble with a `name` column (possibly 0 rows).
#'
#' @export
adjustment_set <- function(cg,
                           X = NULL,
                           Y = NULL,
                           X_index = NULL,
                           Y_index = NULL,
                           type = c("parents", "backdoor", "optimal")) {
  is_caugi(cg, TRUE)
  cg <- build(cg)
  type <- match.arg(type)

  if (length(X) > 1 || length(Y) > 1 ||
    length(X_index) > 1 || length(Y_index) > 1) {
    stop("Provide exactly one X and one Y.",
      call. = FALSE
    )
  }

  X_idx0 <- .resolve_idx0_get(cg@name_index_map, X, X_index)
  Y_idx0 <- .resolve_idx0_get(cg@name_index_map, Y, Y_index)

  idx0 <- switch(type,
    parents = adjustment_set_parents_ptr(cg@ptr, X_idx0, Y_idx0),
    backdoor = adjustment_set_backdoor_ptr(cg@ptr, X_idx0, Y_idx0),
    optimal = adjustment_set_optimal_ptr(cg@ptr, X_idx0, Y_idx0)
  )
  cg@nodes$name[idx0 + 1L]
}

#' @title Is a backdoor set valid?
#'
#' @description Checks whether `Z` is a valid backdoor adjustment set for
#' `X --> Y`.
#' @param cg A `caugi_graph` object.
#'
#' @param X,Y Single node names.
#' @param Z Optional node set for conditioning
#' @param X_index,Y_index,Z_index Optional 1-based indices.
#'
#' @returns Logical scalar.
#' @export
is_valid_backdoor <- function(cg,
                              X = NULL,
                              Y = NULL,
                              Z = NULL,
                              X_index = NULL,
                              Y_index = NULL,
                              Z_index = NULL) {
  is_caugi(cg, TRUE)
  cg <- build(cg)

  X_idx0 <- .resolve_idx0_get(cg@name_index_map, X, X_index)
  Y_idx0 <- .resolve_idx0_get(cg@name_index_map, Y, Y_index)
  Z_idx0 <- .resolve_idx0_mget(cg@name_index_map, Z, Z_index)

  is_valid_backdoor_set_ptr(cg@ptr, X_idx0, Y_idx0, Z_idx0)
}

#' @title Get all backdoor sets up to a certain size.
#'
#' @description This function returns the backdoor sets up to size `max_size`,
#' which per default is set to 10.
#'
#' @param cg A `caugi_graph`.
#' @param X,Y Single node name.
#' @param X_index,Y_index Optional 1-based indices (exclusive with name args).
#' @param minimal Logical; if `TRUE` (default), only minimal sets are returned.
#' @param max_size Integer; maximum size of sets to consider (default 10).
#'
#' @returns A list of character vectors, each an adjustment set
#' (possibly empty).
#' @export
all_backdoor_sets <- function(cg,
                              X = NULL,
                              Y = NULL,
                              X_index = NULL,
                              Y_index = NULL,
                              minimal = TRUE,
                              max_size = 10L) {
  is_caugi(cg, TRUE)
  cg <- build(cg)


  X_idx0 <- .resolve_idx0_get(cg@name_index_map, X, X_index)
  Y_idx0 <- .resolve_idx0_get(cg@name_index_map, Y, Y_index)

  sets_idx0 <- all_backdoor_sets_ptr(
    cg@ptr,
    X_idx0,
    Y_idx0,
    minimal,
    as.integer(max_size)
  )
  nm <- cg@nodes$name
  lapply(sets_idx0, \(idx0) nm[idx0 + 1L])
}

#' @title Resolve node name or index to 0-based index.
#'
#' @description Internal helper function to resolve either a node name or a
#' node index to a 0-based index.
#' `.resolve_idx0_get` uses `get` on the `fastmap` and expects a single value,
#' while `.resolve_idx0_mget` uses `mget` and can return multiple values.
#'
#' @param nm_idx_map A `fastmap` mapping node names to 0-based indices from
#' a `caugi_graph`.
#' @param node_name Optional character vector of node names.
#' @param node_index Optional numeric vector of 1-based node indices.
#'
#' @name .resolve_idx0_get
#'
#' @seealso [fastmap::fastmap]
#' @keywords internal
.resolve_idx0_get <- function(nm_idx_map,
                              node_name = NULL,
                              node_index = NULL) {
  if (!is.null(node_index)) {
    as.integer(node_index - 1L)
  } else if (!is.null(node_name)) {
    if (!is.null(node_index)) {
      stop("Provide either a node name or node index.")
    }
    nm_idx_map$get(node_name, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(node_name, nm_idx_map$keys()),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either the node name or the node index must be provided.",
      call. = FALSE
    )
  }
}

#' @name .resolve_idx0_get
#' @keywords internal
.resolve_idx0_mget <- function(nm_idx_map,
                               node_name = NULL,
                               node_index = NULL) {
  if (is.null(node_name) && is.null(node_index)) {
    integer(0)
  } else if (!is.null(node_index)) {
    as.integer(node_index - 1L)
  } else if (!is.null(node_name)) {
    if (!is.null(node_index)) {
      stop("Provide either a node name or node index.")
    }
    as.integer(
      nm_idx_map$mget(node_name,
        missing = stop(
          paste(
            "Non-existant node name:",
            paste(setdiff(node_name, nm_idx_map$keys()),
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      )
    )
  }
}
