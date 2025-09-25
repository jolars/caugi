#' @title Run `cargo test` in the Rust source directory manually.
#'
#' @description This function runs `cargo test` in the specified Rust source
#' directory using processx.
#'
#' @param path Character. The path to the Rust source directory.
#' Default is "./src/rust".
#'
#' @returns The output of processx::run command
#'
#' @keywords internal
run_cargo_test <- function(path = "./src/rust") {
  res <- processx::run(
    command = "cargo",
    args = c("test"),
    wd = path,
    echo = TRUE
  )
}

#' @title Roclet to run cargo tests
#'
#' @description A roclet that runs `cargo test` in the Rust source directory
#' when roxygen is run.
#'
#' @export
cargo_test_roclet <- function() {
  roxygen2::roclet("cargo_test")
}

#' @title Process for the cargo tests roclet
#'
#' @description
#' This function is a no-op because the actual work is done in the
#' `roclet_output`.
#'
#' @exportS3Method roxygen2::roclet_process roclet_cargo_test
roclet_process.roclet_cargo_test <- function(x, blocks, env, base_path) {
  invisible()
}

#' @title Output for the cargo tests roclet
#'
#' @description
#' This function runs `cargo test` in the Rust source directory if a
#' `Cargo.toml` file is found there.
#'
#' @exportS3Method roxygen2::roclet_output roclet_cargo_test
roclet_output.roclet_cargo_test <- function(x, results, base_path, ...) {
  rust_dir <- file.path(base_path, "src", "rust")
  if (!file.exists(file.path(rust_dir, "Cargo.toml"))) {
    return(invisible())
  }

  env <- c(Sys.getenv(), RUST_BACKTRACE = "1")
  res <- processx::run("cargo", c("test", "--color", "always"),
    wd = rust_dir, env = env,
    echo = TRUE, error_on_status = FALSE,
    windows_verbatim_args = TRUE
  )
  if (res$status != 0) stop("cargo test failed", call. = FALSE)
  invisible()
}

#' @title Clean for the cargo tests roclet
#'
#' @description
#' This function is a no-op because there is nothing to clean up.
#'
#' @exportS3Method roxygen2::roclet_clean roclet_cargo_test
roclet_clean.roclet_cargo_test <- function(x, base_path) {
  invisible()
}

#' @title Combined Coverage Report for R and Rust Code
#'
#' @description
#' Generates a combined code coverage report for R and Rust code in a package
#' using `covr`. This function runs coverage analysis for both R and Rust code,
#' merges the results, and produces a unified coverage report. Currently, it
#' does not support coverage from R tests of Rust code.
#'
#' @param pkg Character. The path to the R package.
#' Default is the current directory `"."`.
#' @param rust_dir Character. The path to the Rust source directory within the
#' package.
#' @param run_r Logical. Whether to run coverage for R code. Defaults to TRUE.
#' @param run_rust Logical. Whether to run coverage for Rust code.
#' Defaults to TRUE.
#' @param ignore Character vector. File patterns to ignore in the coverage
#' report. For example, ignoring `lib.rs` gives a more meaningful coverage
#' estimate. Defaults to `character()`.
#' @param ... Additional arguments passed to `covr::report()`.
#'
#' @returns Invisibly returns a `covr` coverage object.
#' @export
coverage_report <- function(pkg = ".",
                            rust_dir = "src/rust",
                            run_r = TRUE,
                            run_rust = TRUE,
                            ignore = character(),
                            ...) {
  rlang::check_installed("covr")
  cov <- rust_r_covr(pkg, rust_dir, run_r, run_rust)
  if (is.list(cov)) {
    covr::report(cobertura_to_covr(cov$r, cov$rust, ..., ignore = ignore))
  } else if (grepl("<coverage", cov, fixed = TRUE) || grepl("rust.xml$", cov)) {
    covr::report(cobertura_to_covr(NULL, cov, ..., ignore = ignore))
  } else {
    covr::report(cobertura_to_covr(cov, NULL, ..., ignore = ignore))
  }
}

#' @title Convert Cobertura XML to covr Coverage Object
#'
#' @description
#' Converts Cobertura XML coverage reports from R and Rust code into a `covr`
#' coverage object. This function reads the Cobertura XML, aggregates line
#' coverage data, and constructs a `covr` coverage object that can be used for
#' further analysis or reporting.
#'
#' @param r_xml Character or NULL. Cobertura XML content or file path for R code
#' coverage. If NULL, R coverage is not included.
#' @param rust_xml Character or NULL. Cobertura XML content or file path for
#' Rust code coverage. If NULL, Rust coverage is not included.
#' @param pkg Character. The package directory of the package. Defaults to `"."`.
#' @param rust_dir Character. The directory of the Rust source code within the
#' package. Defaults to `"src/rust"`.
#' @param ignore Character vector. File patterns to ignore in the coverage
#' report. Defaults to `character()`.
#'
#' @returns A `covr` coverage object.
#' @keywords internal
cobertura_to_covr <- function(r_xml,
                              rust_xml = NULL,
                              pkg = ".",
                              rust_dir = file.path(pkg, "src/rust"),
                              ignore = character()) {
  rlang::check_installed(c("xml2", "covr"))
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  esc <- function(s) gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", s)

  # ignore patterns: default is glob; use "re:<regex>" for raw regex
  compile_ignores <- function(pats) {
    if (!length(pats)) {
      return(character())
    }
    vapply(pats, function(p) {
      if (startsWith(p, "re:")) {
        sub("^re:", "", p)
      } else {
        # if no wildcard provided, treat as substring glob "*p*"
        has_wild <- grepl("[*?[]", p)
        utils::glob2rx(if (has_wild) p else paste0("*", p, "*"))
      }
    }, character(1))
  }
  ignore_rx <- compile_ignores(ignore)
  any_ignored <- function(path) {
    if (!length(ignore_rx)) {
      return(FALSE)
    }
    any(vapply(ignore_rx, function(rx) grepl(rx, path), logical(1)))
  }

  read_cob <- function(input) {
    if (is.null(input)) {
      return(list())
    }
    x <- tryCatch(
      {
        if (length(input) == 1L && file.exists(input)) {
          xml2::read_xml(input)
        } else {
          xml2::read_xml(input)
        }
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

  root_n <- norm(pkg)
  rust_n <- norm(rust_dir)
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
    if (any_ignored(f)) next

    da <- A[[f]] %||% data.frame(line = integer(), hits = integer())
    db <- B[[f]] %||% data.frame(line = integer(), hits = integer())
    agg <- rbind(da, db)
    if (!nrow(agg)) next

    real <- resolve_real(f)
    disp <- display_path(real, f)
    if (any_ignored(disp) || (!is.na(real) && any_ignored(norm(real)))) next

    agg <- agg[order(agg$line), , drop = FALSE]
    if (is.na(real)) next
    lines <- tryCatch(readLines(real, warn = FALSE),
      error = function(e) character()
    )
    sf <- srcfilecopy(disp, lines)
    for (i in seq_len(nrow(agg))) {
      ln <- agg$line[i]
      if (ln < 1L || ln > length(lines)) next
      sr <- base::srcref(sf, c(ln, 1L, ln, 1L, 1L, 1L, ln, ln))
      out[[length(out) + 1L]] <- structure(
        list(
          value = as.numeric(agg$hits[i]), srcref = sr,
          functions = NA_character_
        ),
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


#' @title Generate Coverage Reports for R and Rust Code
#'
#' @description
#' Generates coverage reports for both R and Rust code in a package using `covr`
#' and `cargo llvm-cov`. It runs coverage analysis for R code using `covr`
#' and for Rust code using `cargo llvm-cov`, then combines the results into a
#' list containing Cobertura XML strings for both R and Rust coverage.
#'
#' @param pkg Character. The path to the R package. Default is the current
#' directory `"."`.
#' @param rust_dir Character. The path to the Rust source directory within the
#' package. Default is `"src/rust"`.
#' @param run_r Logical. Whether to run coverage for R code. Defaults to TRUE.
#' @param run_rust Logical. Whether to run coverage for Rust code.
#' Defaults to TRUE.
#'
#' @returns A list with elements `r` and `rust` containing Cobertura XML strings
#' for R and Rust coverage, respectively. If only one type of coverage is run,
#' it returns just the Cobertura XML string for that type.
#' @keywords internal
rust_r_covr <- function(pkg = ".",
                        rust_dir = "src/rust",
                        run_r = TRUE,
                        run_rust = TRUE) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  ensure <- function(bin, msg) {
    if (nzchar(Sys.which(bin)) == FALSE) {
      stop(msg, call. = FALSE)
    }
  }
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
    if (!requireNamespace("covr", quietly = TRUE)) {
      stop("covr not installed.",
        call. = FALSE
      )
    }
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
    st <- run("cargo", c(
      "llvm-cov",
      "--workspace",
      "--cobertura",
      "--output-path",
      tf2
    ),
    wd = rust_dir
    )
    if (st != 0) {
      stop("cargo llvm-cov failed. Ensure cargo-llvm-cov is installed.",
        call. = FALSE
      )
    }
    rust_xml_txt <- paste(readLines(tf2, warn = FALSE), collapse = "\n")
  }

  if (is.null(r_xml_txt) && is.null(rust_xml_txt)) {
    stop("No coverage artifacts produced.", call. = FALSE)
  }
  if (!is.null(r_xml_txt) && is.null(rust_xml_txt)) {
    return(r_xml_txt)
  }
  if (is.null(r_xml_txt) && !is.null(rust_xml_txt)) {
    return(rust_xml_txt)
  }
  invisible(list(r = r_xml_txt, rust = rust_xml_txt))
}
