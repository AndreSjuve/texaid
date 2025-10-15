#' Format a metadata block as a commented header
#' @param metadata named list of scalars (characterable)
#' @param path target file (used to infer extension)
#' @param marker_begin,marker_end unique markers for idempotent replacement
#' @return character vector of lines (ready to write)
format_metadata_block <- function(
  metadata,
  path,
  marker_begin = "==META BEGIN==",
  marker_end = "==META END=="
) {
  stopifnot(is.list(metadata), !is.null(names(metadata)))
  ext <- tolower(fs::path_ext(path))
  prefix <- .comment_prefix_for(ext)

  # Normalize values to single-line strings
  kv <- vapply(
    metadata,
    function(x) {
      if (length(x) == 0 || is.na(x)) "" else as.character(x[[1]])
    },
    FUN.VALUE = character(1)
  )

  # Two special cases: md/qmd -> HTML comment block
  if (ext %in% c("md", "qmd")) {
    lines <- c(
      paste0("<!-- ", marker_begin, " -->"),
      sprintf("<!-- %s: %s -->", names(kv), kv),
      paste0("<!-- ", marker_end, " -->")
    )
    return(lines)
  }

  # Generic line-comment formats
  if (!is.na(prefix)) {
    # For YAML-like, we still comment to avoid altering document semantics
    lines <- c(
      sprintf("%s %s", prefix, marker_begin),
      sprintf("%s %s: %s", prefix, names(kv), kv),
      sprintf("%s %s", prefix, marker_end),
      "" # blank line after header
    )
    return(lines)
  }

  # Unsupported inline format -> return empty; caller will sidecar
  character()
}
