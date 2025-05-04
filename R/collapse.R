#' Collapse mutual directed edges (-->, not o->) to a single edge (--- or <->)
#'
#' @param df A tibble with columns from, to, edge_type
#' @param collapse_to The code to use for collapsed directed pairs (default "---")
#' @return A tibble with directed pairs collapsed
#' @keywords internal
collapse_directed_to_symmetric_edges <- function(df, collapse_to = "---") {
  stopifnot(is.data.frame(df), "edge_type" %in% names(df))

  # 1) identify the mutual (-->) pairs
  key <- paste(df$from, df$to, sep = "\r")
  rev_key <- paste(df$to, df$from, sep = "\r")
  is_sym <- key %in% rev_key

  # 2) asymmetric ones (keep exactly as-is)
  df_asym <- df[!is_sym, , drop = FALSE]

  # 3) symmetric ones (i→j & j→i), collapse one per unordered pair
  df_sym2 <- if (any(is_sym)) {
    df_sym <- df[is_sym, , drop = FALSE]
    # build unique unordered keys, but *preserve* the correct ordering
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
    # no symmetric pairs
    df[FALSE, , drop = FALSE]
  }

  # 4) recombine & sort for reproducibility
  out <- rbind(df_asym, df_sym2)
  out[order(out$from, out$to), , drop = FALSE]
}

#' Collapse mutual directed PAG edges (o->, not -->) to a single o-o edge
#'
#' @param df A tibble with columns from, to, edge_type
#' @param collapse_to The code to use for collapsed directed pairs (default "---")
#' @return A tibble with directed pairs collapsed
#' @keywords internal
collapse_directed_PAG_to_symmetric_PAG <- function(df) {
  stopifnot(
    is.data.frame(df)
  )

  # split directed vs other
  df_dir <- df |> filter(edge_type == "o->")
  df_other <- df |> filter(!edge_type == "o->")

  # collapse mutual directed pairs
  df_dir_collapsed <-
    df_dir |>
    mutate(u = pmin(from, to), v = pmax(from, to)) |>
    group_by(u, v) |>
    summarize(
      edge_type = if (n() > 1) o - o else first(edge_type),
      .groups = "drop"
    ) |>
    transmute(from = u, to = v, edge_type)

  bind_rows(df_other, df_dir_collapsed) |>
    arrange(from, to)
}

#' Collapse bidirected (<->) and undirected (---) edges to single edges
#'
#' @param df A tibble with columns from, to, edge_type
#' @return A tibble with symmetric edges collapsed
#' @keywords internal
collapse_symmetric_edges <- function(df) {
  symmetric_codes <- c("<->", "o-o", "---")
  # split symmetric vs other
  df_sym <- df |> filter(edge_type %in% symmetric_codes)
  df_other <- df |> filter(!edge_type %in% symmetric_codes)

  # collapse duplicates of symmetric codes
  df_sym_collapsed <-
    df_sym |>
    mutate(u = pmin(from, to), v = pmax(from, to)) |>
    distinct(u, v, edge_type) |>
    transmute(from = u, to = v, edge_type)

  bind_rows(df_other, df_sym_collapsed) |>
    arrange(from, to)
}
