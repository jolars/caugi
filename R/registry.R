# ──────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────── Registry API ─────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Access the global edge registry, creating it if needed.
#'
#' @description The global edge registry is created on first access, and
#' built-in edge types are registered once per session.
#' This function is called internally by `caugi_graph()`.
#'
#' @returns An `edge_registry` external pointer.
#'
#' @keywords internal
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
#'
#' @description This is mainly useful for testing purposes to clear the edge
#' registry.
#'
#' @returns TRUE, invisibly.
#'
#' @export
reset_caugi_registry <- function() {
  .caugi_env$reg <- NULL
  .caugi_env$edge_ops <- .caugi_defaults_edge_ops
  .caugi_env$glyph_map <- .caugi_defaults_glyph_map
  invisible(TRUE)
}

#' @title Register a new edge type in the global registry.
#'
#' @description This function allows you to register a new edge type (glyph)
#' in the global edge registry used by `caugi_graph()`.
#'
#' @param glyph A string representing the edge glyph (e.g., `"-->"`, `"<->"`).
#' @param left_mark A string representing the left mark type.
#' Possible values: "line", "circle", "arrow", "other".
#' @param right_mark A string representing the right mark type.
#' Possible values: "line", "circle", "arrow", "other".
#' @param orientation A string representing the edge orientation.
#' Possible values: "none", "right_head", "left_head", "both_heads".
#' @param class A string representing the edge class.
#' Possible values: "directed", "undirected", "bidirected", "partial".
#' @param symmetric Logical; if TRUE, the edge is considered symmetric.
#' @param traversable_when_conditioned Logical; if TRUE, the edge can be
#' traversed, when conditioning on the nodes that it connects.
#'
#' @returns The integer code assigned to the registered edge type.
#'
#' @export
register_caugi_edge <- function(glyph,
                                left_mark,
                                right_mark,
                                orientation,
                                class,
                                symmetric = FALSE,
                                traversable_when_conditioned = TRUE) {
  reg <- caugi_registry()
  edge_registry_register(
    reg,
    glyph,
    left_mark,
    right_mark,
    orientation,
    class,
    symmetric,
    traversable_when_conditioned
  )
  .register_edge(glyph)
}

#' @title Seal the global edge registry.
#'
#' @description After sealing, no further edge types can be registered.
#'
#' @returns TRUE, invisibly.
#'
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
#'
#' @description A tibble of the built-in edge specifications.
#'
#' @returns A tibble with columns `glyph`, `left_mark`, `right_mark`,
#' `orientation`, `class`, `symmetric`, and `traversable_when_conditioned`.
#'
#' @keywords internal
.caugi_builtin_specs <- function() {
  tibble::tibble(
    glyph = c("-->", "---", "<->", "o-o", "o--", "o->"),
    left_mark = c("line", "line", "arrow", "circle", "circle", "circle"),
    right_mark = c("arrow", "line", "arrow", "circle", "line", "arrow"),
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
