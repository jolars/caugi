#' Collapse mutual directed edges (-->) to a single symmetric edge (--- or <->)
#'
#' @param df         Tibble with columns `from`, `to`, `edge_type`
#' @param collapse_to Code to use for collapsed pairs (default "---")
#' @return           Tibble with collapsed edge list
#' @importFrom dplyr filter mutate transmute arrange bind_rows distinct
#' @importFrom rlang .data
#' @keywords internal
collapse_directed_to_symmetric_edges <- function(df, collapse_to = "---") {
  stopifnot(is.data.frame(df), "edge_type" %in% names(df))

  # 1) identify the mutual (-->) pairs
  key <- paste(df$from, df$to, sep = "\r")
  rev_key <- paste(df$to, df$from, sep = "\r")
  is_sym <- key %in% rev_key

  # 2) asymmetric ones: keep as-is
  df_asym <- df[!is_sym, , drop = FALSE]

  # 3) symmetric pairs: collapse to one edge
  df_sym2 <- if (any(is_sym)) {
    df_sym <- df[is_sym, , drop = FALSE]

    # build unique unordered keys while preserving canonical ordering
    pairs <- unique(paste(
      ifelse(df_sym$from < df_sym$to, df_sym$from, df_sym$to),
      ifelse(df_sym$from < df_sym$to, df_sym$to, df_sym$from),
      sep = "\r"
    ))

    # turn each key back into a single row
    do.call(rbind, lapply(pairs, function(k) {
      nodes <- strsplit(k, "\r", fixed = TRUE)[[1]]
      data.frame(
        from = nodes[1],
        to = nodes[2],
        edge_type = collapse_to,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    df[FALSE, , drop = FALSE] # no symmetric pairs
  }

  # 4) recombine & sort for reproducibility
  out <- rbind(df_asym, df_sym2)
  out[order(out$from, out$to), , drop = FALSE]
}

#' Collapse mutual directed PAG edges (o->) to a single o-o edge
#'
#' @param df Tibble with columns `from`, `to`, `edge_type`
#' @return   Tibble with collapsed edge list
#' @importFrom dplyr filter mutate group_by summarize transmute arrange bind_rows
#' @importFrom rlang .data
#' @keywords internal
collapse_directed_PAG_to_symmetric_PAG <- function(df) {
  stopifnot(is.data.frame(df))

  # split directed vs other
  df_dir <- dplyr::filter(df, .data$edge_type == "o->")
  df_other <- dplyr::filter(df, .data$edge_type != "o->")

  # collapse mutual directed pairs
  df_dir_collapsed <-
    df_dir |>
    dplyr::mutate(
      u = pmin(.data$from, .data$to),
      v = pmax(.data$from, .data$to)
    ) |>
    dplyr::group_by(.data$u, .data$v) |>
    dplyr::summarize(
      edge_type = if (dplyr::n() > 1) "o-o" else dplyr::first(.data$edge_type),
      .groups   = "drop"
    ) |>
    dplyr::transmute(from = .data$u, to = .data$v, edge_type = .data$edge_type)

  dplyr::bind_rows(df_other, df_dir_collapsed) |>
    dplyr::arrange(.data$from, .data$to)
}

#' Collapse bidirected (<->) and undirected (---) duplicates
#'
#' @param df Tibble with columns `from`, `to`, `edge_type`
#' @return   Tibble with duplicates removed
#' @importFrom dplyr filter mutate transmute distinct arrange bind_rows
#' @importFrom rlang .data
#' @keywords internal
collapse_symmetric_edges <- function(df) {
  symmetric_codes <- c("<->", "o-o", "---")

  # split symmetric vs other
  df_sym <- dplyr::filter(df, .data$edge_type %in% symmetric_codes)
  df_other <- dplyr::filter(df, !.data$edge_type %in% symmetric_codes)

  # collapse duplicates
  df_sym_collapsed <-
    df_sym |>
    dplyr::mutate(
      u = pmin(.data$from, .data$to),
      v = pmax(.data$from, .data$to)
    ) |>
    dplyr::distinct(.data$u, .data$v, .data$edge_type) |>
    dplyr::transmute(from = .data$u, to = .data$v, edge_type = .data$edge_type)

  dplyr::bind_rows(df_other, df_sym_collapsed) |>
    dplyr::arrange(.data$from, .data$to)
}
