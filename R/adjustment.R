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

  xi0 <- .resolve_idx_adjustment(
    cg,
    substitute(X),
    X_index,
    "X",
    allow_empty = FALSE,
    env = parent.frame()
  )
  yi0 <- .resolve_idx_adjustment(
    cg,
    substitute(Y),
    Y_index,
    "Y",
    allow_empty = FALSE,
    env = parent.frame()
  )
  zi0 <- if (missing(Z) && missing(Z_index)) {
    integer(0)
  } else {
    .resolve_idx_adjustment(
      cg,
      substitute(Z),
      Z_index,
      "Z",
      allow_empty = TRUE,
      env = parent.frame()
    )
  }

  is_d_separated_ptr(cg@ptr, xi0, yi0, zi0)
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
#' @param X,Y Node selectors for exposures and outcomes.
#' Use `*_index` to pass indices.
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

  X0 <- .resolve_idx_adjustment(
    cg,
    substitute(X),
    X_index,
    "X",
    allow_empty = FALSE,
    env = parent.frame()
  )
  Y0 <- .resolve_idx_adjustment(
    cg,
    substitute(Y),
    Y_index,
    "Y",
    allow_empty = FALSE,
    env = parent.frame()
  )

  idx0 <- switch(type,
    parents = adjustment_set_parents_ptr(cg@ptr, X0, Y0),
    backdoor = adjustment_set_backdoor_ptr(cg@ptr, X0, Y0),
    optimal = {
      if (length(X0) != 1L || length(Y0) != 1L) {
        stop("For type = 'optimal', provide exactly one X and one Y.",
          call. = FALSE
        )
      }
      adjustment_set_optimal_ptr(cg@ptr, X0[[1]], Y0[[1]])
    }
  )
  .getter_output(cg, idx0)
}

#' @title Is a backdoor set valid?
#'
#' @description Checks whether `Z` is a valid backdoor adjustment set for
#' `X --> Y`.
#' @param cg A `caugi_graph` object.
#'
#' @param x,y Single node (name/expression)
#' or 1-based index via `x_index`,`y_index`.
#' @param Z Optional node set for conditioning; or `Z_index` for indices.
#' @param x_index,y_index,Z_index Optional 1-based indices
#' (exclusive with name args).
#'
#' @returns Logical scalar.
#' @export
is_valid_backdoor <- function(cg,
                              x = NULL,
                              y = NULL,
                              Z = NULL,
                              x_index = NULL,
                              y_index = NULL,
                              Z_index = NULL) {
  is_caugi(cg, TRUE)
  cg <- build(cg)

  x0 <- .resolve_idx_adjustment(
    cg,
    substitute(x),
    x_index, "x",
    allow_empty = FALSE,
    env = parent.frame()
  )
  y0 <- .resolve_idx_adjustment(
    cg,
    substitute(y),
    y_index,
    "y",
    allow_empty = FALSE,
    env = parent.frame()
  )
  if (length(x0) != 1L || length(y0) != 1L) {
    stop("Provide exactly one x and one y.", call. = FALSE)
  }
  z0 <- if (missing(Z) && missing(Z_index)) {
    integer(0)
  } else {
    .resolve_idx_adjustment(
      cg,
      substitute(Z),
      Z_index,
      "Z",
      allow_empty = TRUE,
      env = parent.frame()
    )
  }

  is_valid_backdoor_set_ptr(cg@ptr, x0[[1]], y0[[1]], z0)
}

#' @title Enumerate minimal backdoor sets
#'
#' @description Enumerates minimal valid backdoor sets for the pair `x -> y`.
#'
#' @param cg A `caugi_graph`.
#' @param x,y Single node (name/expression) or via `x_index`,`y_index`.
#' @param x_index,y_index Optional 1-based indices (exclusive with name args).
#' @param minimal Logical; if `TRUE` (default), only minimal sets are returned.
#' @param max_size Integer; maximum size of sets to consider (default 10).
#'
#' @returns A list of character vectors, each an adjustment set
#' (possibly empty).
#' @export
all_backdoor_sets <- function(cg,
                              x = NULL,
                              y = NULL,
                              x_index = NULL,
                              y_index = NULL,
                              minimal = TRUE,
                              max_size = 10L) {
  is_caugi(cg, TRUE)
  cg <- build(cg)

  x0 <- .resolve_idx_adjustment(
    cg,
    substitute(x),
    x_index,
    "x",
    allow_empty = FALSE,
    env = parent.frame()
  )
  y0 <- .resolve_idx_adjustment(
    cg,
    substitute(y),
    y_index,
    "y",
    allow_empty = FALSE,
    env = parent.frame()
  )
  if (length(x0) != 1L || length(y0) != 1L) {
    stop("Provide exactly one x and one y.", call. = FALSE)
  }

  sets_idx0 <- all_backdoor_sets_ptr(
    cg@ptr,
    x0[[1]],
    y0[[1]],
    minimal,
    max_size
  )

  .getter_output(cg, sets_idx0)
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Helpers ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# Like .relations(), but returns 0-based integer vector and accepts NULL.
# nodes can be character or unquoted expression; index is 1-based numeric.
# Exactly one of (nodes, index) must be supplied unless allow_empty = TRUE.
# If allow_empty and both are missing/NULL, returns integer(0).
#' @title Resolve nodes or indices
#'
#' @description Internal helper to resolve nodes or indices.
#'
#' @param cg A `caugi_graph` object.
#' @param nodes Node selector: character vector of names or unquoted expression
#' (supports `+` and `c()`). Exclusive with `index`.
#' @param index Optional numeric 1-based indices (exclusive with `nodes`).
#' @param what Character; description of what is being resolved (for error
#' messages).
#' @param allow_empty Logical; if `TRUE`, allows both `nodes` and `index` to be
#' missing/`NULL`, returning `integer(0)`.
#'
#' @returns Integer vector of 0-based indices.
#'
#' @keywords internal
.resolve_idx_adjustment <- function(cg, nodes, index, what,
                                    allow_empty = FALSE,
                                    env = parent.frame()) {
  index_supplied <- !missing(index) && !is.null(index)
  expr_supplied <- !missing(nodes) && !is.null(nodes)

  if (expr_supplied && index_supplied) {
    stop("For ", what, ", supply either names/expression or indices, not both.", call. = FALSE)
  }
  if (!expr_supplied && !index_supplied) {
    if (allow_empty) {
      return(integer(0))
    }
    stop("Missing ", what, ".", call. = FALSE)
  }

  if (index_supplied) {
    return(.resolve_idx_from_index(cg, index))
  }

  val <- try(eval(nodes, env), silent = TRUE)
  if (!inherits(val, "try-error")) {
    if (is.list(val) && length(val) == 1L && is.character(val[[1]])) {
      return(.resolve_idx(cg, val[[1]]))
    }
    if (is.character(val)) {
      return(.resolve_idx(cg, val))
    }
  }

  .resolve_idx(cg, .expand_nodes(nodes, env))
}
