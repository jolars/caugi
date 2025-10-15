# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────── Load S4 classes ────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

# We load S4 classes from inst as a workaround to avoid putting S4 classes
# in the Imports field of the DESCRIPTION file, which suggests that the package
# actually depends on them, which it doesn't.

graphNEL_S4_class <- readRDS("inst/S4_class_definitions/graphNEL_class.rds")
Matrix_S4_class <- readRDS("inst/S4_class_definitions/Matrix_class.rds")

#' @title Convert to a `caugi_graph`
#'
#' @description Convert an object to a `caugi_graph`. The object can be a
#' `graphNEL`, ``matrix`, or a data frame with `from`, `to`, and
#' `edge_type` columns.
#'
#' @details
#' For matrices, `as_caugi` assumes that the rows are the `from` nodes
#' and the columns are the `to` nodes. Thus, for a graph, G: A --> B, we would
#' have that `G["A", "B"] == 1` and `G["B", "A"] == 0`.
#' For PAGs, the integer codes are as follows (as used in `pcalg`):
#' - 0: no edge
#' - 1: tail (e.g., `A o-- B` or `A --- B`)
#' - 2: arrowhead (e.g., `A --> B` or `A o-> B`)
#' - 3: circle (e.g., `A o-o B`)
#'
#' @param x An object to convert to a `caugi_graph`.
#' @param class "DAG", "PDAG", "PAG", or "Unknown".
#' "PAG" is only supported for integer coded matrices.
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
#' @returns A `caugi_graph` object.
#'
#' @export
as_caugi <- S7::new_generic(
  "as_caugi",
  dispatch_args = "x",
  fun = function(x,
                 class = c("DAG", "PDAG", "PAG", "Unknown"),
                 simple = TRUE,
                 build = TRUE,
                 collapse = FALSE,
                 collapse_to = "---",
                 ...) {
    S7::S7_dispatch()
  }
)


#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  S7::new_S3_class("igraph")
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  class <- match.arg(class)

  n_edges <- igraph::ecount(x)
  directed <- igraph::is_directed(x)
  nm <- igraph::V(x)$name
  if (is.null(nm)) nm <- paste0("V", seq_len(igraph::vcount(x)))

  if (class == "PAG") {
    stop("PAG class is not supported for igraph objects.", call. = FALSE)
  }
  if (n_edges == 0L) {
    return(caugi_graph(
      from = character(),
      edge = character(),
      to = character(),
      nodes = nm,
      simple = isTRUE(simple),
      build = isTRUE(build),
      class = class
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

    # keep asymmetric rows and the first canonical rep of each symmetric group
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
    nodes = nm,
    simple = isTRUE(simple),
    build = isTRUE(build),
    class = class
  )
}

#' @name as_caugi
#' @export
S7::method(
  as_caugi, graphNEL_S4_class
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  class <- match.arg(class)

  directed <- graph::isDirected(x)

  nbrs <- graph::edges(x)
  lens <- vapply(nbrs, length, integer(1))
  from <- rep.int(names(nbrs), lens)
  to <- unlist(nbrs, use.names = FALSE)
  nm <- x@nodes
  if (is.null(nm)) nm <- paste0("V", seq_len(length(nbrs)))

  if (!directed) {
    canon_from <- pmin(from, to)
    canon_to <- pmax(from, to)
    keep <- !duplicated(interaction(canon_from, canon_to, drop = TRUE))
    from <- canon_from[keep]
    to <- canon_to[keep]
  }

  # no edges
  if (length(from) == 0L) {
    return(caugi_graph(
      from = character(),
      edge = character(),
      to = character(),
      nodes = nm,
      simple = isTRUE(simple),
      build = isTRUE(build),
      class = class
    ))
  }

  glyph <- if (directed) "-->" else "---"
  edge <- rep_len(glyph, length(from))

  if (collapse && directed) {
    is_edge_symmetric(collapse_to)

    canon_from <- pmin(from, to)
    canon_to <- pmax(from, to)
    key <- interaction(canon_from, canon_to, drop = TRUE)

    pair_key <- interaction(from, to, drop = TRUE)
    rev_key <- interaction(to, from, drop = TRUE)
    has_rev <- (from != to) & match(pair_key, rev_key, nomatch = 0L) > 0L

    keep <- !has_rev | (from == canon_from)

    from <- ifelse(has_rev & keep, canon_from, from)[keep]
    to <- ifelse(has_rev & keep, canon_to, to)[keep]
    edge <- ifelse(has_rev & keep, collapse_to, edge)[keep]
  }

  caugi_graph(
    from = from,
    edge = edge,
    to = to,
    nodes = nm,
    simple = isTRUE(simple),
    build = isTRUE(build),
    class = class
  )
}


#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  S7::new_S3_class("integer")
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  if (!is.matrix(x)) {
    stop("`x` must be a matrix.", call. = FALSE)
  }

  class <- match.arg(class)

  if (nrow(x) != ncol(x)) {
    stop("`x` must be a square adjacency matrix.", call. = FALSE)
  }

  n <- nrow(x)
  x <- as.matrix(x)

  # accept logical or numeric; nonzero means edge
  if (is.logical(x)) {
    if (class == "PAG") {
      stop("PAG class is not supported for logical matrices.", call. = FALSE)
    }
  } else if (is.numeric(x)) {
    if (!all(x == floor(x)) || any(x < 0)) {
      stop("`x` must be a logical or a non-negative integer matrix.",
        call. = FALSE
      )
    }
    if (class == "PAG" && any(!(x %in% 0:3))) {
      stop("PAG class is only supported for integer codes 0-3.",
        "This is meant to represent PAG edge types as used in `pcalg`.",
        call. = FALSE
      )
    }
    if (class != "PAG" && any(!(x %in% 0:1))) {
      stop("Only 0 and 1 integer codes are supported for non-PAG matrices.",
        call. = FALSE
      )
    }
  } else {
    stop("`x` must be logical or numeric matrix.", call. = FALSE)
  }

  # handle naming
  nm <- colnames(x)
  if (is.null(nm)) nm <- rownames(x)
  if (is.null(nm)) {
    if (n > 0) nm <- paste0("V", seq_len(n))
  }

  # helpers
  mark_sym <- function(k) {
    switch(as.character(k),
      "1" = "-",
      "2" = ">",
      "3" = "o",
      stop("Invalid PAG code: ", k, call. = FALSE)
    )
  }

  # build edge list
  if (class %in% "PAG") {
    mark_sym <- function(k) {
      switch(as.character(k),
        "1" = "-",
        "2" = ">",
        "3" = "o",
        stop("Invalid PAG code: ", k, call. = FALSE)
      )
    }

    from <- character(0)
    to <- character(0)
    edge <- character(0)

    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        a <- x[i, j] # mark at j-end
        b <- x[j, i] # mark at i-end
        if (a == 0L && b == 0L) next

        lf <- mark_sym(b) # left mark (at i)
        rt <- mark_sym(a) # right mark (at j)

        # enforce right-pointing arrows only; swap if needed
        if (lf == ">" && rt != ">") {
          tmp <- lf
          lf <- rt
          rt <- tmp
          ii <- i
          i <- j
          j <- ii
        }
        if (lf == "o" && rt == "-") {
          tmp <- lf
          lf <- rt
          rt <- tmp
          ii <- i
          i <- j
          j <- ii
        }

        glyph <- switch(paste0(lf, rt),
          "--" = "---",
          "->" = "-->",
          "-o" = "--o",
          "o>" = "o->",
          "oo" = "o-o",
          ">>" = "<->", # bidirected allowed
          stop("Unsupported PAG endpoints: ", lf, " .. ", rt, call. = FALSE)
        )

        from <- c(from, nm[i])
        to <- c(to, nm[j])
        edge <- c(edge, glyph)
      }
    }
  } else { # DAG / PDAG / Unknown: nonzero means directed i -> j
    idx <- which(x != 0, arr.ind = TRUE)
    if (nrow(idx) == 0L) {
      return(caugi_graph(
        from = character(),
        edge = character(),
        to = character(),
        nodes = nm,
        simple = isTRUE(simple),
        build = isTRUE(build),
        class = class
      ))
    }

    from <- nm[idx[, 1L]]
    to <- nm[idx[, 2L]]
    edge <- rep_len("-->", nrow(idx))

    if (collapse) {
      is_edge_symmetric(collapse_to)

      canon_from <- pmin(from, to)
      canon_to <- pmax(from, to)
      key <- interaction(canon_from, canon_to, drop = TRUE)

      pair_key <- interaction(from, to, drop = TRUE)
      rev_key <- interaction(to, from, drop = TRUE)
      has_rev <- (from != to) & match(pair_key, rev_key, nomatch = 0L) > 0L

      keep <- !has_rev | (from == canon_from)

      from <- ifelse(has_rev & keep, canon_from, from)[keep]
      to <- ifelse(has_rev & keep, canon_to, to)[keep]
      edge <- ifelse(has_rev & keep, collapse_to, edge)[keep]
    }
  }

  # todo:
  ### NEEDS TO BE FIXED ONCE PAG IS SUPPORTED IN CAUGI ###
  class <- if (class == "PAG") "Unknown" else class
  ### NEEDS TO BE FIXED ONCE PAG IS SUPPORTED IN CAUGI ###

  caugi_graph(
    from = from,
    edge = edge,
    to = to,
    nodes = nm,
    simple = isTRUE(simple),
    build = isTRUE(build),
    class = class
  )
}

#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  S7::new_S3_class("double")
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  if (!is.matrix(x)) {
    stop("`x` must be a matrix.", call. = FALSE)
  }
  if (!all(x == floor(x)) || any(x < 0)) {
    stop("`x` must be a logical or a non-negative integer matrix.",
      call. = FALSE
    )
  }
  class <- match.arg(class)

  storage.mode(x) <- "integer"
  as_caugi(x,
    class = class,
    simple = simple,
    build = build,
    collapse = collapse,
    collapse_to = collapse_to,
    ...
  )
}

#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  S7::new_S3_class("logical")
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  if (!is.matrix(x)) {
    stop("`x` must be a matrix.", call. = FALSE)
  }
  class <- match.arg(class)
  if (class == "PAG") {
    stop("PAG class is not supported for logical matrices.", call. = FALSE)
  }
  storage.mode(x) <- "integer"
  as_caugi(x,
    class = class,
    simple = simple,
    build = build,
    collapse = collapse,
    collapse_to = collapse_to,
    ...
  )
}

#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  Matrix_S4_class
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  class <- match.arg(class)
  m <- as.matrix(x)
  as_caugi(
    m,
    class = class,
    simple = simple,
    build = build,
    collapse = collapse,
    collapse_to = collapse_to,
    ...
  )
}

#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  S7::new_S3_class("tidygraph")
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  class <- match.arg(class)
  if (class == "PAG") {
    stop("PAG class is not supported for tidygraph objects.", call. = FALSE)
  }
  as_caugi(
    tidygraph::as.igraph(x),
    class = class,
    simple = simple,
    build = build,
    collapse = collapse,
    collapse_to = collapse_to,
    ...
  )
}

#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  S7::new_S3_class("dagitty")
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  class <- match.arg(class)

  nm <- names(x) |> as.vector()

  # edges() returns data frame with columns v, w, e
  ed <- as.data.frame(dagitty::edges(x), stringsAsFactors = FALSE)
  if (nrow(ed) == 0L) {
    # collect declared nodes, including isolates if available
    nm <- names(x)
    return(caugi_graph(
      from = character(),
      edge = character(),
      to = character(),
      nodes = nm,
      simple = isTRUE(simple),
      build = isTRUE(build),
      class = if (class == "PAG") "Unknown" else class
    ))
  }

  v <- as.character(ed$v)
  w <- as.character(ed$w)
  e <- as.character(ed$e)

  # normalize direction when dagitty encodes reverse
  e <- gsub("@", "o", e, fixed = TRUE)

  # swap reverse encodings (anything starting with "<-")
  rev <- grepl("^<-", e)
  if (any(rev)) {
    tmp <- v[rev]
    v[rev] <- w[rev]
    w[rev] <- tmp
    e[rev] <- sub("^<-o$", "o->", sub("^<-$", "->", e[rev]))
  }

  map_glyph <- function(s) {
    switch(s,
      "->" = "-->",
      "<->" = "<->",
      "--" = "---",
      "o->" = "o->",
      "o-o" = "o-o",
      stop("Unsupported dagitty edge code: ", s, call. = FALSE)
    )
  }
  edge <- vapply(e, map_glyph, character(1L))

  # canonicalize undirected to avoid duplicates
  und <- edge == "---"
  if (any(und)) {
    cf <- pmin(v[und], w[und])
    ct <- pmax(v[und], w[und])
    keep <- !duplicated(paste0(cf, "\r", ct))
    v[und] <- cf
    w[und] <- ct
    edge[und][!keep] <- NA_character_
  }

  keep <- !is.na(edge)
  from <- v[keep]
  to <- w[keep]
  edge <- edge[keep]

  # todo:
  ### NEEDS TO BE FIXED ONCE PAG IS SUPPORTED IN CAUGI ###
  class <- if (class == "PAG") "Unknown" else class
  ### NEEDS TO BE FIXED ONCE PAG IS SUPPORTED IN CAUGI ###

  # optional collapse of mutual directed pairs to a symmetric glyph
  if (collapse) {
    is_edge_symmetric(collapse_to)

    pair <- paste0(from, "\r", to)
    revp <- paste0(to, "\r", from)
    has_rev <- (from != to) & match(pair, revp, nomatch = 0L) > 0L

    cf <- pmin(from, to)
    ct <- pmax(from, to)
    canon <- paste0(cf, "\r", ct)
    keep_rep <- !duplicated(canon) & (from == cf)

    keep2 <- !has_rev | keep_rep
    from <- ifelse(has_rev & keep2, cf, from)[keep2]
    to <- ifelse(has_rev & keep2, ct, to)[keep2]
    edge <- ifelse(has_rev & keep2, collapse_to, edge)[keep2]
  }

  caugi_graph(
    from = from,
    edge = edge,
    to = to,
    nodes = nm,
    simple = isTRUE(simple),
    build = isTRUE(build),
    class = class
  )
}

#' @name as_caugi
#' @export
S7::method(
  as_caugi,
  S7::new_S3_class("bn")
) <- function(x,
              class = c("DAG", "PDAG", "PAG", "Unknown"),
              simple = TRUE,
              build = TRUE,
              collapse = FALSE,
              collapse_to = "---",
              ...) {
  class <- match.arg(class)

  if (class == "PAG") {
    stop("PAG class is not supported for bnlearn objects.", call. = FALSE)
  }

  nm <- bnlearn::nodes(x)
  ar <- bnlearn::arcs(x)

  # no edges
  if (nrow(ar) == 0L) {
    return(caugi_graph(
      from = character(),
      edge = character(),
      to = character(),
      nodes = nm,
      simple = isTRUE(simple),
      build = isTRUE(build),
      class = class
    ))
  }

  from <- as.character(ar[, 1L])
  to <- as.character(ar[, 2L])
  edge <- rep_len("-->", length.out = nrow(ar))

  # optional collapse (bnlearn DAGs should not need it)
  if (collapse) {
    is_edge_symmetric(collapse_to)
    pair <- paste0(from, "\r", to)
    revp <- paste0(to, "\r", from)
    has_rev <- (from != to) & match(pair, revp, nomatch = 0L) > 0L
    cf <- pmin(from, to)
    ct <- pmax(from, to)
    canon <- paste0(cf, "\r", ct)
    keep_rep <- !duplicated(canon) & (from == cf)
    keep <- !has_rev | keep_rep
    from <- ifelse(has_rev & keep, cf, from)[keep]
    to <- ifelse(has_rev & keep, ct, to)[keep]
    edge <- ifelse(has_rev & keep, collapse_to, edge)[keep]
  }

  caugi_graph(
    from = from,
    edge = edge,
    to = to,
    nodes = nm,
    simple = isTRUE(simple),
    build = isTRUE(build),
    class = class
  )
}
