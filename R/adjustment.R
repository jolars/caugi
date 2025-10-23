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

  X_idx0 <- if (!is.null(X_index)) {
    as.integer(X_index - 1L)
  } else if (!is.null(X)) {
    if (!is.null(X_index)) {
      stop("Provide only one of `X` or `X_index`.")
    }
    cg@name_index_map$get(X, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(X, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either X or X_index must be provided.", call. = FALSE)
  }

  Y_idx0 <- if (!is.null(Y_index)) {
    as.integer(Y_index - 1L)
  } else if (!is.null(Y)) {
    if (!is.null(Y_index)) {
      stop("Provide only one of `Y` or `Y_index`.")
    }
    cg@name_index_map$get(Y, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(Y, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either Y or Y_index must be provided.", call. = FALSE)
  }

  Z_idx0 <- if (is.null(Z) && is.null(Z_index)) {
    integer(0)
  } else if (!is.null(Z_index)) {
    as.integer(Z_index - 1L)
  } else if (!is.null(Z)) {
    if (!is.null(Z_index)) {
      stop("Provide only one of `Z` or `Z_index`.")
    }
    as.integer(
      cg@name_index_map$mget(Z,
        missing = stop(
          paste(
            "Non-existant node name:",
            paste(setdiff(Z, cg@nodes$name),
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      )
    )
  }

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

  X_idx0 <- if (!is.null(X_index)) {
    as.integer(X_index - 1L)
  } else if (!is.null(X)) {
    if (!is.null(X_index)) {
      stop("Provide only one of `X` or `X_index`.")
    }
    cg@name_index_map$get(X, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(X, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either X or X_index must be provided.", call. = FALSE)
  }

  Y_idx0 <- if (!is.null(Y_index)) {
    as.integer(Y_index - 1L)
  } else if (!is.null(Y)) {
    if (!is.null(Y_index)) {
      stop("Provide only one of `Y` or `Y_index`.")
    }
    cg@name_index_map$get(Y, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(Y, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either Y or Y_index must be provided.", call. = FALSE)
  }


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

  X_idx0 <- if (!is.null(X_index)) {
    as.integer(X_index - 1L)
  } else if (!is.null(X)) {
    if (!is.null(X_index)) {
      stop("Provide only one of `X` or `X_index`.")
    }
    cg@name_index_map$get(X, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(X, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either X or X_index must be provided.", call. = FALSE)
  }

  Y_idx0 <- if (!is.null(Y_index)) {
    as.integer(Y_index - 1L)
  } else if (!is.null(Y)) {
    if (!is.null(Y_index)) {
      stop("Provide only one of `Y` or `Y_index`.")
    }
    cg@name_index_map$get(Y, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(Y, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either Y or Y_index must be provided.", call. = FALSE)
  }

  Z_idx0 <- if (is.null(Z) && is.null(Z_index)) {
    integer(0)
  } else if (!is.null(Z_index)) {
    as.integer(Z_index - 1L)
  } else if (!is.null(Z)) {
    if (!is.null(Z_index)) {
      stop("Provide only one of `Z` or `Z_index`.")
    }
    as.integer(
      cg@name_index_map$mget(Z,
        missing = stop(
          paste(
            "Non-existant node name:",
            paste(setdiff(Z, cg@nodes$name),
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      )
    )
  }

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

  X_idx0 <- if (!is.null(X_index)) {
    as.integer(X_index - 1L)
  } else if (!is.null(X)) {
    cg@name_index_map$get(X, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(X, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either X or X_index must be provided.", call. = FALSE)
  }

  Y_idx0 <- if (!is.null(Y_index)) {
    as.integer(Y_index - 1L)
  } else if (!is.null(Y)) {
    cg@name_index_map$get(Y, missing = stop(
      paste(
        "Non-existant node name:",
        paste(setdiff(Y, cg@nodes$name),
          collapse = ", "
        )
      ),
      call. = FALSE
    ))
  } else {
    stop("Either Y or Y_index must be provided.", call. = FALSE)
  }

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
