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
