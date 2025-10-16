#' Save a LaTeX table to disk with metadata (direct write, simplified)
#'
#' @description
#' Captures LaTeX output, writes it directly to disk, injects metadata, and
#' optionally prints/copies the LaTeX for quick inspection.
#'
#' @inheritParams ltx_save_table
#' @param tbl_path Character scalar. Destination `.tex` path (required).
#' @param preprocess Optional function (e.g., `ltx_environment`) to modify the
#'   LaTeX lines before saving. Default `NULL`.
#' @param add_metadata Logical, inject provenance metadata. Default `TRUE`.
#' @param overwrite Logical, overwrite existing file. Default `TRUE`.
#' @param create_dirs Logical, create parent directories if needed. Default `TRUE`.
#' @param print_tbl Logical, print LaTeX to console and clipboard. Default `TRUE`.
#' @param quiet Logical, suppress informational messages. Default `FALSE`.
#'
#' @return Invisibly returns the written LaTeX lines with the written path as
#'   attribute `"path"`.
#'
#' @export
ltx_save_table <- function(
  x,
  tbl_path,
  add_metadata = TRUE,
  overwrite = TRUE,
  create_dirs = TRUE,
  print_tbl = TRUE,
  quiet = FALSE,
  preprocess = NULL
) {
  # ── Validate path ────────────────────────────────────────────────────────────
  if (
    missing(tbl_path) ||
      is.null(tbl_path) ||
      !is.character(tbl_path) ||
      length(tbl_path) != 1L ||
      !nzchar(tbl_path)
  ) {
    cli::cli_abort(
      "{.arg tbl_path} must be a single non-empty file path (character)."
    )
  }

  dir_path <- fs::path_dir(tbl_path)
  if (!fs::dir_exists(dir_path)) {
    if (isTRUE(create_dirs)) {
      if (!quiet) {
        cli::cli_inform("Creating directory: {dir_path}")
      }
      fs::dir_create(dir_path, recurse = TRUE)
    } else {
      cli::cli_abort("Directory does not exist: {dir_path}")
    }
  }

  if (fs::file_exists(tbl_path) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "File already exists and {.arg overwrite} is FALSE: {tbl_path}"
    )
  }

  # ── Capture LaTeX lines ─────────────────────────────────────────────────────
  lines <- if (is.character(x)) {
    unlist(
      strsplit(paste(x, collapse = "\n"), "\n", fixed = TRUE),
      use.names = FALSE
    )
  } else {
    ltx_capture_output(x)
  }

  lines <- sub("[ \t]+$", "", lines)

  # ── Optional preprocessing ──────────────────────────────────────────────────
  if (!is.null(preprocess)) {
    if (!is.function(preprocess)) {
      cli::cli_abort("{.arg preprocess} must be a function or NULL.")
    }
    lines <- preprocess(lines)
  }

  # ── Direct write ────────────────────────────────────────────────────────────
  write_tex(x = lines, path = tbl_path, verbose = FALSE)

  # ── Metadata ────────────────────────────────────────────────────────────────
  if (isTRUE(add_metadata)) {
    meta <- try(
      suppressWarnings(collect_output_metadata(output_path = tbl_path)),
      silent = TRUE
    )
    if (!inherits(meta, "try-error")) {
      try(write_metadata_block(tbl_path, meta, mode = "auto"), silent = TRUE)
    } else {
      cli::cli_warn("Metadata collection failed; continuing without metadata.")
    }
  }

  if (!quiet) {
    cli::cli_inform("Saved table: {tbl_path}")
  }

  # ── Read back and print ─────────────────────────────────────────────────────
  out <- readLines(tbl_path, warn = FALSE)
  if (isTRUE(print_tbl)) {
    ltx_print_tbl(out, print = TRUE)
  }

  attr(out, "path") <- tbl_path
  invisible(out)
}
