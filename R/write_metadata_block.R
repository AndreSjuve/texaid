#' Write or inject a metadata block for a generated artefact
#'
#' For text formats with line comments (e.g., .tex, .csv, .R), injects a
#' commented block at the top of the file between begin/end markers. Replaces
#' an existing block if present (idempotent). For unsupported/binary formats,
#' writes a sidecar YAML file (<basename>.meta.yml).
#'
#' @param path Path to the artefact file.
#' @param metadata Named list returned by `collect_output_metadata()`.
#' @param mode "auto", "inline", or "sidecar". In "auto", the function selects
#'   inline for supported text formats and sidecar otherwise.
#' @param sidecar_ext File extension for sidecar (default "meta.yml").
#' @param marker_begin,marker_end Unique markers used to delimit the block.
#' @return Invisibly returns the path written (file or sidecar).
#' @export
write_metadata_block <- function(
  path,
  metadata,
  mode = c("auto", "inline", "sidecar"),
  sidecar_ext = "meta.yml",
  marker_begin = "==META BEGIN==",
  marker_end = "==META END=="
) {
  mode <- match.arg(mode)
  stopifnot(length(path) == 1L, file.exists(path))
  ext <- tolower(fs::path_ext(path))

  # Try to build an inline block
  inline <- format_metadata_block(metadata, path, marker_begin, marker_end)
  inline_supported <- length(inline) > 0

  if (mode == "sidecar" || (!inline_supported && mode == "auto")) {
    # Write sidecar YAML
    sidecar <- fs::path_ext_set(path, sidecar_ext)
    .write_yaml_sidecar(sidecar, metadata)
    cli::cli_alert_success("Metadata sidecar written: {sidecar}")
    return(invisible(sidecar))
  }

  # Inline injection path
  lines_old <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # Locate existing markers (if any) to replace idempotently
  begin_pat <- paste0("(^|\\s)", marker_begin, "($|\\s)")
  end_pat <- paste0("(^|\\s)", marker_end, "($|\\s)")

  # Build regex depending on comment style
  # For md/qmd we used HTML comments; for others, prefix is in lines already.
  begin_idx <- grep(begin_pat, lines_old, perl = TRUE)
  end_idx <- grep(end_pat, lines_old, perl = TRUE)

  if (length(begin_idx) == 1L && length(end_idx) == 1L && begin_idx < end_idx) {
    # Replace existing block
    lines_new <- c(
      lines_old[seq_len(begin_idx - 1L)],
      inline,
      lines_old[seq(from = end_idx + 1L, to = length(lines_old))]
    )
  } else {
    # Prepend new block
    lines_new <- c(inline, lines_old)
  }

  # Atomic-ish write via temp file
  tmp <- paste0(path, ".tmp")
  writeLines(lines_new, tmp, useBytes = TRUE)
  fs::file_move(tmp, path)

  cli::cli_alert_success("Metadata header injected into: {path}")
  invisible(path)
}
