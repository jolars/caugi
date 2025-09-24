coverage_report <- function(...) {
  rlang::check_installed("covr")
  cov <- rust_r_covr(...)
  if (is.list(cov)) {
    covr::report(cobertura_to_covr(cov$r, cov$rust, ...))
  } else if (grepl("<coverage", cov, fixed = TRUE) || grepl("rust.xml$", cov)) {
    covr::report(cobertura_to_covr(NULL, cov, ...))
  } else {
    covr::report(cobertura_to_covr(cov, NULL, ...))
  }
}

cobertura_to_covr <- function(r_xml, rust_xml = NULL, root = ".", rust_root = file.path(root, "src/rust")) {
  rlang::check_installed(c("xml2", "covr"))
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  esc <- function(s) gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", s)

  read_cob <- function(input) {
    if (is.null(input)) {
      return(list())
    }
    x <- tryCatch(
      {
        if (length(input) == 1L && file.exists(input)) xml2::read_xml(input) else xml2::read_xml(input)
      },
      error = function(e) stop("Invalid Cobertura XML input.", call. = FALSE)
    )
    classes <- xml2::xml_find_all(x, "//class")
    recs <- lapply(classes, function(cls) {
      fn <- xml2::xml_attr(cls, "filename")
      ln <- xml2::xml_find_all(cls, ".//line")
      if (is.na(fn) || !length(ln)) {
        return(NULL)
      }
      data.frame(
        file = fn,
        line = as.integer(xml2::xml_attr(ln, "number")),
        hits = as.integer(xml2::xml_attr(ln, "hits") %||% "0"),
        stringsAsFactors = FALSE
      )
    })
    recs <- recs[!vapply(recs, is.null, logical(1))]
    if (!length(recs)) {
      return(list())
    }
    recs <- do.call(rbind, recs)
    agg <- stats::aggregate(hits ~ file + line, recs, sum)
    split(agg[c("line", "hits")], agg$file)
  }

  A <- read_cob(r_xml)
  B <- read_cob(rust_xml)
  files <- union(names(A), names(B))

  root_n <- norm(root)
  rust_n <- norm(rust_root)
  resolve_real <- function(f) {
    cand <- c(f, file.path(root_n, f), file.path(rust_n, f))
    cand[file.exists(cand)][1] %||% NA_character_
  }
  display_path <- function(real, fallback = NULL) {
    if (is.na(real)) {
      fb <- (fallback %||% "")
      fb <- sub("^\\./", "", fb)
      return(fb)
    }
    p <- norm(real)
    if (startsWith(p, paste0(rust_n, "/")) || identical(p, rust_n)) {
      tail <- sub(paste0("^", esc(rust_n), "/?"), "", p)
      file.path("src", "rust", tail)
    } else if (startsWith(p, paste0(root_n, "/")) || identical(p, root_n)) {
      rel <- sub(paste0("^", esc(root_n), "/?"), "", p)
      sub("^\\./", "", rel)
    } else {
      basename(p)
    }
  }

  out <- list()
  for (f in files) {
    da <- A[[f]] %||% data.frame(line = integer(), hits = integer())
    db <- B[[f]] %||% data.frame(line = integer(), hits = integer())
    agg <- rbind(da, db)
    if (!nrow(agg)) next
    agg <- agg[order(agg$line), , drop = FALSE]

    real <- resolve_real(f)
    disp <- display_path(real, f)
    if (is.na(real)) next
    lines <- tryCatch(readLines(real, warn = FALSE), error = function(e) character())
    sf <- srcfilecopy(disp, lines)
    for (i in seq_len(nrow(agg))) {
      ln <- agg$line[i]
      if (ln < 1L || ln > length(lines)) next
      sr <- base::srcref(sf, c(ln, 1L, ln, 1L, 1L, 1L, ln, ln))
      out[[length(out) + 1L]] <- structure(
        list(value = as.numeric(agg$hits[i]), srcref = sr, functions = NA_character_),
        class = c("expression_coverage", "list")
      )
    }
  }

  pkg_name <- basename(norm(root))
  structure(out,
    class = c("coverage", "list"),
    package = list(package = pkg_name, path = root_n), root = root_n
  )
}

rust_r_covr <- function(pkg = ".", rust_dir = "src/rust", run_r = TRUE, run_rust = TRUE) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  ensure <- function(bin, msg) if (nzchar(Sys.which(bin)) == FALSE) stop(msg, call. = FALSE)
  run <- function(cmd, args, wd = NULL) {
    if (!is.null(wd)) {
      old <- setwd(wd)
      on.exit(setwd(old), add = TRUE)
    }
    out <- system2(cmd, args, stdout = TRUE, stderr = TRUE)
    attr(out, "status") %||% 0L
  }

  r_xml_txt <- NULL
  rust_xml_txt <- NULL

  if (run_r) {
    if (!requireNamespace("covr", quietly = TRUE)) stop("covr not installed.", call. = FALSE)
    cov <- covr::package_coverage(path = pkg, type = "tests", quiet = TRUE)
    tf <- tempfile(fileext = ".xml")
    on.exit(unlink(tf), add = TRUE)
    covr::to_cobertura(cov, file = tf)
    r_xml_txt <- paste(readLines(tf, warn = FALSE), collapse = "\n")
  }

  if (run_rust) {
    ensure("cargo", "cargo not found in PATH.")
    tf2 <- tempfile(fileext = ".xml")
    on.exit(unlink(tf2), add = TRUE)
    st <- run("cargo", c("llvm-cov", "clean", "--workspace"), wd = rust_dir)
    if (st != 0) warning("cargo llvm-cov clean returned non-zero.")
    st <- run("cargo", c("llvm-cov", "--workspace", "--cobertura", "--output-path", tf2), wd = rust_dir)
    if (st != 0) stop("cargo llvm-cov failed. Ensure cargo-llvm-cov is installed.", call. = FALSE)
    rust_xml_txt <- paste(readLines(tf2, warn = FALSE), collapse = "\n")
  }

  if (is.null(r_xml_txt) && is.null(rust_xml_txt)) stop("No coverage artifacts produced.", call. = FALSE)
  if (!is.null(r_xml_txt) && is.null(rust_xml_txt)) {
    return(r_xml_txt)
  }
  if (is.null(r_xml_txt) && !is.null(rust_xml_txt)) {
    return(rust_xml_txt)
  }
  invisible(list(r = r_xml_txt, rust = rust_xml_txt))
}
