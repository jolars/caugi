# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────────── Operations ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Moralize a DAG
#'
#' @description
#' Moralizing a DAG involves connecting all parents of each node and then
#' converting all directed edges into undirected edges.
#'
#' @details
#' This changes the graph from a Directed Acyclic Graph (DAG) to an
#' Undirected Graph (UG), also known as a Markov Graph.
#'
#'
#' @param cg A `caugi` object (DAG).
#'
#' @returns A `caugi` object representing the moralized graph (UG).
#'
#' @examples
#' cg <- caugi(A %-->% C, B %-->% C, class = "DAG")
#' moralize(cg) # A -- B, A -- C, B -- C
#'
#' @family operations
#' @concept operations
#'
#' @export
moralize <- function(cg) {
  is_caugi(cg, throw_error = TRUE)
  if (cg@graph_class != "DAG") {
    stop("moralize() can only be applied to DAGs.", call. = FALSE)
  }

  cg <- build(cg)
  moralized_ptr <- moralize_ptr(cg@ptr)
  moralized_cg <- .view_to_caugi(moralized_ptr, node_names = cg@nodes$name)
  moralized_cg
}

#' @title Get the skeleton of a graph
#'
#' @description
#' The skeleton of a graph is obtained by replacing all directed edges with
#' undirected edges.
#'
#' @details
#' This changes the graph from any class to an Undirected Graph (UG), also known
#' as a Markov Graph.
#'
#' @param cg A `caugi` object. Either a DAG or PDAG.
#'
#' @returns A `caugi` object representing the skeleton of the graph (UG).
#'
#' @examples
#' cg <- caugi(A %-->% B, class = "DAG")
#' skeleton(cg) # A --- B
#'
#' @family operations
#' @concept operations
#'
#' @export
skeleton <- function(cg) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  skeleton_ptr <- skeleton_ptr(cg@ptr)
  skeleton_cg <- .view_to_caugi(skeleton_ptr, node_names = cg@nodes$name)
  skeleton_cg
}

#' @title Project latent variables from a DAG to an ADMG
#'
#' @description
#' Projects out latent (unobserved) variables from a DAG to produce an
#' Acyclic Directed Mixed Graph (ADMG) over the observed variables.
#'
#' @param cg A `caugi` object of class `"DAG"`.
#' @param latents Character vector of latent variable names to project out.
#'
#' @returns A `caugi` object of class `"ADMG"` containing only the observed
#'   variables.
#'
#' @examples
#' # DAG with latent confounder U
#' dag <- caugi(
#'   U %-->% X,
#'   U %-->% Y,
#'   X %-->% Y,
#'   class = "DAG"
#' )
#'
#' # Project out the latent variable
#' admg <- latent_project(dag, latents = "U")
#' # Result: X -> Y, X <-> Y (children of U become bidirected-connected)
#' edges(admg)
#'
#' # DAG with directed path through latent
#' dag2 <- caugi(
#'   X %-->% L,
#'   L %-->% Y,
#'   class = "DAG"
#' )
#'
#' # Project out the latent variable
#' admg2 <- latent_project(dag2, latents = "L")
#' # Result: X -> Y (directed path X -> L -> Y becomes X -> Y)
#' edges(admg2)
#'
#' @family operations
#' @concept operations
#'
#' @export
latent_project <- function(cg, latents) {
  is_caugi(cg, throw_error = TRUE)

  if (!is_dag(cg)) {
    stop("latent_project() can only be applied to DAGs.", call. = FALSE)
  }

  if (!is.character(latents)) {
    stop("`latents` must be a character vector of node names.", call. = FALSE)
  }

  cg <- build(cg)
  node_names <- cg@nodes$name

  # Validate latent names exist
  missing_latents <- setdiff(latents, node_names)
  if (length(missing_latents) > 0L) {
    stop(
      paste0(
        "Unknown latent node(s): ",
        paste(missing_latents, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Get 0-based indices for latent nodes
  latent_indices <- cg@name_index_map$mget(latents)
  latent_indices <- as.integer(unlist(latent_indices, use.names = FALSE))

  # Get observed node names (preserving order)
  is_latent <- node_names %in% latents
  observed_names <- node_names[!is_latent]

  # Call Rust function
  projected_ptr <- latent_project_ptr(cg@ptr, latent_indices)

  # Convert result back to caugi
  .view_to_caugi(projected_ptr, node_names = observed_names)
}

# ──────────────────────────────────────────────────────────────────────────────
# ──────────────────────────── Mutate caugi class ──────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Mutate `caugi` class
#'
#' @description
#' Mutate the `caugi` class from one graph class to another, if possible.
#' For example, convert a `DAG` to a `PDAG`, or a fully directed `caugi` of
#' class `UNKNOWN` to a `DAG`. Throws an error if not possible.
#'
#' @details
#' This function returns a copy of the object, and the original remains
#' unchanged.
#'
#' @param cg A `caugi` object.
#' @param class A character string specifying the new class.
#'
#' @returns A `caugi` object of the specified class.
#'
#' @examples
#' cg <- caugi(A %-->% B, class = "UNKNOWN")
#' cg_dag <- mutate_caugi(cg, "DAG")
#'
#' @family operations
#' @concept operations
#'
#' @export
mutate_caugi <- function(cg, class) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)
  old_class <- cg@graph_class

  if (old_class == class) {
    return(cg)
  }

  if (is_empty_caugi(cg)) {
    return(caugi(class = class))
  }

  is_mutation_possible <- switch(
    class,
    "DAG" = is_dag(cg),
    "PDAG" = is_pdag(cg),
    "UG" = is_ug(cg),
    "UNKNOWN" = TRUE,
    stop(paste0("Unknown target class: ", class))
  )

  if (!is_mutation_possible) {
    stop(paste0(
      "Cannot convert caugi of class '",
      old_class,
      "' to '",
      class,
      "'.",
      call. = FALSE
    ))
  } else {
    return(caugi(
      nodes = nodes(cg),
      edges_df = edges(cg),
      class = class,
      simple = TRUE,
      build = TRUE
    ))
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# ───────────────────────────────── Exogenize ──────────────────────────────────
# ──────────────────────────────────────────────────────────────────────────────

#' @title Exogenize a graph
#'
#' @description
#' Exogenize a graph by removing all ingoing edges to the set of nodes
#' specified (i.e., make the nodes exogenous), as well as joining the
#' parents of the nodes specified to the children of the nodes specified.
#'
#' @param cg A `caugi` object.
#' @param nodes A character vector of node names to exogenize. Must be a subset
#' of the nodes in the graph.
#'
#' @returns A `caugi` object representing the exogenized graph.
#'
#' @examples
#' cg <- caugi(A %-->% B, class = "DAG")
#' exogenize(cg, nodes = "B") # A, B
#'
#' @family operations
#' @concept operations
#'
#' @export
exogenize <- function(cg, nodes) {
  is_caugi(cg, throw_error = TRUE)
  cg <- build(cg)

  if (!cg@simple) {
    stop(
      "`cg` must be a simple graph, due to the nature of the implementation. ",
      "Please open an issue on github if you need this functionality for ",
      "non-simple graphs.",
      call. = FALSE
    )
  }

  if (!is.character(nodes) || length(nodes) == 0) {
    stop(
      "`nodes` must be a non-empty character vector of node names.",
      call. = FALSE
    )
  }

  all_nodes <- nodes(cg)$name

  for (u in nodes) {
    if (!u %in% all_nodes) {
      stop(paste0("Node ", u, " not in graph."), call. = FALSE)
    }

    pa_u <- parents(cg, u) # NULL or character vector
    ch_u <- children(cg, u) # NULL or character vector

    # Step (i): add edges from every parent of u to every child of u
    if (!is.null(pa_u) && !is.null(ch_u)) {
      # cross-product of pa(u) x ch(u)
      grid <- data.table::CJ(from = pa_u, to = ch_u, unique = TRUE)

      # Avoid self-loops (l -> l)
      grid <- grid[from != to]

      if (nrow(grid) > 0L) {
        cg <- add_edges(
          cg,
          from = grid$from,
          edge = rep("-->", nrow(grid)),
          to = grid$to,
          inplace = TRUE
        )
      }
    }

    # Step (ii): delete incoming edges into u (l -> u)
    if (!is.null(pa_u) && length(pa_u) > 0L) {
      cg <- remove_edges(
        cg,
        from = pa_u,
        to = rep(u, length(pa_u)),
        inplace = TRUE
      )
    }
  }

  cg
}
