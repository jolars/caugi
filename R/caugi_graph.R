# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── caugi graph API ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Create a `caugi_graph` from edge expressions.
#'
#' @description Create a `caugi_graph` from a series of edge expressions using
#' infix operators. Nodes can be specified as symbols, strings, or numbers.
#'
#' The following edge operators are supported by default:
#' * `%-->%` for directed edges (A --> B)
#' * `%---%` for undirected edges (A --- B)
#' * `%<->%` for bidirected edges (A <-> B)
#' * `%o->%` for partially directed edges (A o-> B)
#' * `%o--%` for partially undirected edges (A o-- B)
#' * `%o-o%` for partial edges (A o-o B)
#'
#' You can register additional edge types using [register_caugi_edge()].
#'
#' @param ... Edge expressions using the supported infix operators, or
#' nodes given by symbols or strings. Multiple edges can be
#' combined using `+`: `A --> B + C`, indicating an edge from `A` to both `B`
#' and `C`. Nodes can also be grouped using `c(...)` or parentheses.
#' @param simple Logical; if `TRUE` (default), the graph is a simple graph, and
#' the function will throw an error if the input contains parallel edges or
#' self-loops.
#' @param build Logical; if `TRUE` (default), the graph will be built using the
#' Rust backend. If `FALSE`, the graph will not be built, and the Rust backend
#' cannot be used. The graph will build, when queries are made to the graph or
#' if calling [build()]. __Note__: Even if `build = TRUE`, if no edges or
#' nodes are provided, the graph will not be built and the pointer will be
#' `NULL`.
#' @param class Character; one of `"Unknown"`, `"DAG"`, or `"PDAG"`.
#'
#' @returns A [`caugi_graph`] S7 object containing the nodes, edges, and a
#' pointer to the underlying Rust graph structure.
#'
#' @export
caugi_graph <- S7::new_class(
  "caugi_graph",
  parent = S7::S7_object,
  properties = list(
    `.state` = S7::new_property(S7::class_environment),
    nodes = S7::new_property(
      S7::class_any,
      getter = function(self) self@`.state`$nodes,
      setter = function(self, value) {
        stop("nodes is read-only via @ <-. ",
          "Use `add_nodes()` or `remove_nodes()` instead. ",
          "Advanced users can modify `cg@.state$nodes` directly, ",
          "but this is not recommended.",
          call. = FALSE
        )
      }
    ),
    edges = S7::new_property(
      S7::class_any,
      getter = function(self) self@`.state`$edges,
      setter = function(self, value) {
        stop("`edges` properrt is read-only via @ <-. ",
          "Use `add_edges()` or `remove_edges()` instead. ",
          "Advanced users can modify `cg@.state$edges` directly ",
          "but this is not recommended.",
          call. = FALSE
        )
      }
    ),
    ptr = S7::new_property(
      S7::class_any,
      getter = function(self) self@`.state`$ptr,
      setter = function(self, value) {
        stop("`ptr` property is read-only via @ <-. ",
          "Use `build()` to (re)build the graph.",
          call. = FALSE
        )
      }
    ),
    simple = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        # we would like to return the Rust state if built
        if (is.null(self@ptr)) {
          return(self@`.state`$simple)
        }
        # extra precuation
        if (self@`.state`$simple != is_simple_ptr(self@ptr)) {
          stop(paste0(
            "Internal warning: graph simplicity mismatch between R (",
            self@`.state`$simple, ") and Rust (", is_simple_ptr(self@ptr),
            "). ",
            "Please report this issue."
          ), call. = FALSE)
        }
        is_simple_ptr(self@ptr)
      }
    ),
    built = S7::new_property(
      S7::class_logical,
      getter = function(self) self@`.state`$built,
      setter = function(self, value) {
        stop("`built` property is read-only via @ <-. ",
          "It should only be set at construction or when calling `build()`.",
          call. = FALSE
        )
      }
    ),
    graph_class = S7::new_property(
      S7::class_character,
      getter = function(self) {
        # we would like to return the Rust state if built
        if (is.null(self@ptr)) {
          return(self@`.state`$class)
        }
        # extra precuation
        if (toupper(self@`.state`$class) != toupper(graph_class_ptr(self@ptr))) {
          stop(paste0(
            "Internal warning: graph class mismatch between R (",
            self@`.state`$class, ") and Rust (", graph_class_ptr(self@ptr),
            "). ",
            "Please report this issue."
          ), call. = FALSE)
        }
        graph_class_ptr(self@ptr)
      },
      setter = function(self, value) {
        stop("`graph_class` property is read-only via @ <-. ",
          "It should only be set at construction.",
          call. = FALSE
        )
      }
    )
  ),
  validator = function(self) {
    s <- self@`.state`
    if (isFALSE(s$simple) && !identical(s$class, "Unknown")) {
      return("If simple = FALSE, class must be 'Unknown'")
    }

    if (is.null(self@ptr) && s$built) {
      return(paste0(
        "Internal error: graph pointer is NULL but built = TRUE. ",
        "Please report this issue."
      ))
    }

    # if the graph has been built and the pointer exists, check consistency
    if (!is.null(self@ptr) && s$built) {
      if (toupper(graph_class_ptr(self@ptr)) != toupper(s$class)) {
        return(paste0(
          "Internal error: graph class mismatch between R (", s$class,
          ") and Rust (", graph_class_ptr(self@ptr), "). ",
          "Please report this issue."
        ))
      }
      if (is_simple_ptr(self@ptr) != s$simple) {
        return(paste0(
          "Internal error: graph simplicity mismatch between R (", s$simple,
          ") and Rust (", is_simple_ptr(self@ptr), "). ",
          "Please report this issue."
        ))
      }
    }

    # todo: how may a user corrupt via setting cg@.state$field <- value?
    NULL
  },
  constructor = function(...,
                         simple = TRUE,
                         build = TRUE,
                         class = c("Unknown", "DAG", "PDAG")) {
    class <- match.arg(class)
    calls <- as.list(substitute(list(...)))[-1L]

    if (!simple && class != "Unknown") {
      stop("If simple = FALSE, class must be 'Unknown'", call. = FALSE)
    }

    # Parse calls into edges + declared nodes
    terms <- .collect_edges_nodes(calls)
    edges <- terms$edges
    declared <- terms$declared

    # All unique nodes
    nodes <- tibble::tibble(name = unique(c(edges$from, edges$to, declared)))
    n <- nrow(nodes)
    id <- setNames(seq_len(n) - 1L, nodes$name)

    # Initialize caugi registry (if not already registered)
    reg <- caugi_registry()

    # Initialize graph pointer. If build = FALSE, the graph will not be built
    # and the pointer will be NULL
    gptr <- NULL

    # Monitor if the graph has been built
    built <- FALSE

    # Build the graph using the Rust backend
    if (build && n > 0L) {
      b <- graph_builder_new(reg, n = n, simple = simple)

      if (nrow(edges)) {
        codes <- vapply(
          edges$edge,
          function(g) edge_registry_code_of(reg, g),
          integer(1L)
        )
        graph_builder_add_edges(
          b,
          as.integer(unname(id[edges$from])),
          as.integer(unname(id[edges$to])),
          as.integer(codes)
        )
      }

      # build + wrap with class
      gptr <- graph_builder_build_view(b, class)
      built <- TRUE
    }

    edges <- tibble::tibble(
      from = edges$from,
      edge = edges$edge,
      to = edges$to
    ) |>
      dplyr::arrange(from, to, edge)

    S7::new_object(
      caugi_graph,
      `.state` = .cg_state(
        nodes  = nodes,
        edges  = edges,
        ptr    = gptr,
        built  = built,
        simple = simple,
        class  = class
      )
    )
  }
)

#' @title Internal: Create the state environment for a `caugi_graph`
#'
#' @description Internal function to create the state environment for a
#' `caugi_graph`. This function is not intended to be used directly by users.
#'
#' @param nodes A tibble of nodes with a `name` column.
#' @param edges A tibble of edges with `from`, `edge`, and `to` columns.
#' @param ptr A pointer to the underlying Rust graph structure
#' (or `NULL` if not built).
#' @param built Logical; whether the graph has been built.
#' @param simple Logical; whether the graph is simple
#' (no parallel edges or self-loops).
#' @param class Character; one of `"Unknown"`, `"DAG"`, or `"PDAG"`.
#'
#' @keywords internal
.cg_state <- function(nodes, edges, ptr, built, simple, class) {
  e <- new.env(parent = emptyenv())
  e$nodes <- tibble::tibble(name = nodes$name)
  e$edges <- tibble::tibble(from = edges$from, edge = edges$edge, to = edges$to)
  e$ptr <- ptr
  e$built <- isTRUE(built)
  e$simple <- isTRUE(simple)
  e$class <- class
  e
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── methods ────────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' Length of a `caugi_graph`
#'
#' @description Returns the number of nodes in the graph.
#'
#' @param cg A `caugi_graph` object.
#'
#' @name length
#' @returns An integer representing the number of nodes.
#' @export
S7::method(length, caugi_graph) <- function(x) {
  nrow(x@nodes)
}

#' Print a `caugi_graph`
#'
#' @param cg A `caugi_graph` object.
#' @param ... Not used.
#'
#' @name print
#' @export
S7::method(print, caugi_graph) <- function(cg, ...) {
  print(cg@nodes)
  print(cg@edges)
  invisible(cg)
}
