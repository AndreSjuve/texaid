#' Read metadata block or sidecar from a generated artefact
#'
#' Parses and returns the metadata previously written with
#' \code{\link{write_metadata_block}()}.
#' Works for inline commented headers (e.g. .tex, .csv, .R) as well as for
#' YAML sidecar files (<basename>.meta.yml).
#'
#' @param path Path to the artefact file (or its sidecar).
#' @param marker_begin,marker_end The markers used when the metadata was written.
#' @param sidecar_ext The sidecar extension to look for if no inline block exists.
#'
#' @return A named character vector (metadata fields) or an empty list if none found.
#' @export
read_metadata_block <- function(
  path,
  marker_begin = "==META BEGIN==",
  marker_end = "==META END==",
  sidecar_ext = "meta.yml"
) {
  stopifnot(length(path) == 1L)

  # Helper: parse key-value pairs from comment lines
  parse_comment_lines <- function(lines) {
    # Strip comment prefix and whitespace
    stripped <- sub("^\\s*([#%]\\s*|<!--\\s*|\\s*-->)?", "", lines)
    # Split on first colon
    parts <- strsplit(stripped, ":", fixed = TRUE)
    kv <- lapply(parts, function(x) {
      key <- trimws(x[1])
      val <- trimws(paste(x[-1], collapse = ":"))
      c(key, val)
    })
    kv <- Filter(function(x) length(x) == 2 && nzchar(x[1]), kv)
    out <- vapply(kv, function(x) x[2], character(1))
    names(out) <- vapply(kv, function(x) x[1], character(1))
    out
  }

  # 1. Check for inline metadata
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    begin_idx <- grep(marker_begin, lines, fixed = TRUE)
    end_idx <- grep(marker_end, lines, fixed = TRUE)

    if (
      length(begin_idx) == 1L && length(end_idx) == 1L && begin_idx < end_idx
    ) {
      block_lines <- lines[(begin_idx + 1):(end_idx - 1)]
      meta <- parse_comment_lines(block_lines)
      if (length(meta)) return(meta)
    }
  }

  # 2. If no inline block, try sidecar
  sidecar <- fs::path_ext_set(path, sidecar_ext)
  if (file.exists(sidecar)) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      return(yaml::read_yaml(sidecar))
    } else {
      # Simple key: value parser
      lines <- readLines(sidecar, warn = FALSE, encoding = "UTF-8")
      parts <- strsplit(lines, ":", fixed = TRUE)
      kv <- Filter(function(x) length(x) >= 2, parts)
      out <- vapply(
        kv,
        function(x) trimws(paste(x[-1], collapse = ":")),
        character(1)
      )
      names(out) <- vapply(kv, function(x) trimws(x[1]), character(1))
      return(out)
    }
  }

  # 3. If not found
  cli::cli_alert_info("No metadata found for file: {path}")
  return(list())
}
