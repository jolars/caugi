# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────── Registry API ─────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Access the global edge registry, creating it if needed.
#' @returns An `edge_registry` external pointer.
caugi_registry <- function() {
  if (!exists("reg", envir = .caugi_env, inherits = FALSE) ||
    is.null(.caugi_env$reg)) {
    reg <- edge_registry_new()
    edge_registry_register_builtins(reg) # once per session
    .caugi_env$reg <- reg
  }
  .caugi_env$reg
}

#' @title Reset the global edge registry.
#' @export
reset_caugi_registry <- function() {
  .caugi_env$reg <- NULL
  .caugi_env$edge_ops <- .caugi_defaults_edge_ops
  .caugi_env$glyph_map <- .caugi_defaults_glyph_map
  invisible(TRUE)
}

#' @title Register a new edge type in the global registry.
#'
#' @param glyph A string representing the edge glyph (e.g., `"-->"`, `"<->"`).
#' @param orientation One of "none","right_head","left_head","both_heads".
#' @param class One of "directed","undirected","bidirected","partial".
#' @param symmetric Logical.
#' @param traversable_when_conditioned Logical.
#' @returns The integer code assigned to the registered edge type.
#' @export
register_caugi_edge <- function(glyph,
                                orientation,
                                class,
                                symmetric = FALSE,
                                traversable_when_conditioned = TRUE) {
  reg <- caugi_registry()
  edge_registry_register(
    reg,
    glyph,
    orientation,
    class,
    symmetric,
    traversable_when_conditioned
  )
  .register_edge(glyph)
}

#' @title Seal the global edge registry.
#' @export
seal_caugi_registry <- function() {
  reg <- caugi_registry()
  edge_registry_seal(reg)
  invisible(TRUE)
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── Registry helpers ───────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Built-in edge specifications
#' @description A tibble of the built-in edge specifications.
#' @returns A tibble with columns `glyph`, `orientation`, `class`,
#' `symmetric`, and `traversable_when_conditioned`.
#' @keywords internal
.caugi_builtin_specs <- function() {
  tibble::tibble(
    glyph = c("-->", "---", "<->", "o-o", "--o", "o->"),
    orientation = c(
      "right_head", "none", "both_heads", "none", "none", "right_head"
    ),
    class = c(
      "directed", "undirected", "bidirected", "undirected", "partial", "partial"
    ),
    symmetric = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
    traversable_when_conditioned = TRUE
  )
}

#' @title Register a new edge operator
#'
#' @description Register a new edge operator for use in `caugi_graph()`.
#'
#' @param glyph A string representing the edge glyph (e.g., `"-->"`, `"<->"`).
#' @param where An environment to register the operator in
#' (default: `.GlobalEnv`).
#'
#' @returns The operator name (e.g., `"%-->%"`), invisibly.
#'
#' @keywords internal
.register_edge <- function(glyph, where = .GlobalEnv) {
  if (!is.environment(where)) {
    stop("where must be an environment")
  }
  if (!is.character(glyph) || length(glyph) != 1L) {
    stop("glyph must be a single string")
  }
  if (nchar(glyph) != 3L) {
    stop("glyph must be a non-empty string of length 3 (e.g., '-->')")
  }
  if (grepl("%", glyph)) {
    stop("glyph must not contain '%'")
  }
  op <- paste0("%", glyph, "%")

  # ensure the operator can call the internal .edge_spec
  ns <- topenv(environment(.edge_spec))

  # check if in the global registry
  if (op %in% .edge_ops_get()) {
    stop("Operator ", op, " is already registered")
  }

  f <- local({
    g <- glyph
    function(lhs, rhs) .edge_spec(substitute(lhs), substitute(rhs), g)
  })
  environment(f) <- ns

  assign(op, f, envir = where)

  .caugi_env$edge_ops <- unique(c(.edge_ops_get(), op))
  gm <- .glyph_map_get()
  gm[[op]] <- glyph
  .caugi_env$glyph_map <- gm
  invisible(op)
}
