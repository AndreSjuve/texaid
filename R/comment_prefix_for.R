#' Get the line comment prefix for a given file extension
#'
#' Returns the appropriate line comment prefix used for metadata headers or
#' inline annotations based on the file extension. This helper is primarily
#' used internally by functions that inject metadata blocks into text files
#' (e.g., LaTeX, CSV, R scripts, Markdown).
#'
#' @param ext Character scalar giving the file extension (without leading dot).
#'   The lookup is case-insensitive.
#'
#' @return
#' A character string containing the comment prefix to use for the given file
#' type (for example, `"#"` or `"%"`).
#' Returns `NA_character_` if the extension is not recognized.
#'
#' @details
#' The following mappings are defined:
#'
#' | Extension(s) | Comment prefix | Notes |
#' |---------------|----------------|-------|
#' | `tex` | `%` | LaTeX/TikZ files |
#' | `r`, `s`, `rscript`, `py` | `#` | R, S, and Python scripts |
#' | `csv`, `tsv`, `txt`, `yaml`, `yml` | `#` | Data or config files |
#' | `md`, `qmd` | `<!--` | Markdown / Quarto (handled as HTML comments) |
#'
#' File types that do not support inline comments (e.g., `.png`, `.pdf`) should
#' use sidecar metadata files instead.
#'
#' @examples
#' \dontrun{
#' .comment_prefix_for("tex")
#' #> "%"
#'
#' .comment_prefix_for("csv")
#' #> "#"
#'
#' .comment_prefix_for("md")
#' #> "<!--"
#'
#' .comment_prefix_for("png")
#' #> NA_character_
#'}
#' @seealso [format_metadata_block()], [write_metadata_block()]
#'   for higher-level metadata writing functions that rely on this helper.
#'
#' @keywords internal
#' @noRd
.comment_prefix_for <- function(ext) {
  ext <- tolower(ext)
  switch(
    ext,
    "tex" = "%",
    "r" = "#",
    "s" = "#",
    "rscript" = "#",
    "py" = "#",
    "csv" = "#",
    "tsv" = "#",
    "txt" = "#",
    "yaml" = "#",
    "yml" = "#",
    "md" = "<!--", # handled specially
    "qmd" = "<!--", # handled specially
    NA_character_
  )
}
