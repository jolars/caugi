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
#' @details
#' The latent projection is a fundamental operation in causal inference for
#' converting a DAG with latent variables to an ADMG representing the marginal
#' independence structure over observed variables only.
#'
#' The algorithm:
#' 1. Removes all latent nodes from the graph.
#' 2. For each pair of observed nodes (X, Y) **without an existing directed edge**,
#'    adds a bidirected edge X <-> Y if and only if they share a latent ancestor
#'    (i.e., An(X) ∩ An(Y) ∩ Latents is non-empty).
#' 3. Preserves directed edges between observed nodes.
#'
#' Note: Since simple graphs cannot have parallel edges, a bidirected edge is
#' NOT added between nodes that already have a directed edge. The directed edge
#' takes precedence.
#'
#' Note: The resulting ADMG will have nodes re-indexed, preserving the
#' relative order of observed nodes from the original DAG.
#'
#' @param cg A `caugi` object of class `"DAG"`.
#' @param latents Character vector of latent variable names to project out.
#'
#' @returns A `caugi` object of class `"ADMG"` containing only the observed
#'   variables, with directed edges preserved and bidirected edges added where
#'   nodes share latent ancestors.
#'
#' @references
#' Evans, R. J. (2015). *Graphs for Margins of Bayesian Networks*.
#' arXiv:1408.1809, Sections 3-4.
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
#' # Result: X -> Y, X <-> Y (confounded direct effect)
#'
#' edges(admg)
#'
#' @family operations
#' @concept operations
#'
#' @export
latent_project <- function(cg, latents) {
  is_caugi(cg, throw_error = TRUE)

  if (cg@graph_class != "DAG") {
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
