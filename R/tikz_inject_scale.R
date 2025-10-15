#' Inject a scale argument into a TikZ picture
#'
#' This utility modifies a `.tex` file produced by \pkg{tikzDevice}
#' (or a similar exporter) by inserting or replacing a `scale` argument
#' in the `\\begin{tikzpicture}` line.
#' The function is useful when the generated figure appears too large or
#' too small in the final LaTeX document and you prefer to adjust its
#' overall scale without re-rendering the plot.
#'
#' @param path Character string giving the path to the `.tex` file
#'   containing the TikZ picture.
#' @param scale Numeric scalar specifying the scaling factor to apply,
#'   e.g. `0.8` scales the entire picture to 80% of its original size.
#'
#' @details
#' The function searches for the first line in the file that begins with
#' `\\begin{tikzpicture}` and:
#' \itemize{
#'   \item Adds a new `scale=<value>` argument if none exists;
#'   \item Replaces an existing `scale=` argument if present.
#' }
#' The modification is performed in-place; the file is read, updated, and
#' written back to disk.
#'
#' The width and height specified when calling
#' \code{tikzDevice::tikz()} are preserved; scaling affects the entire
#' TikZ coordinate system uniformly, including text and line widths.
#'
#' @return
#' Invisibly returns \code{TRUE} if a `\\begin{tikzpicture}` statement was
#' found and updated, or \code{FALSE} if no modification was made.
#'
#' @examples
#' \dontrun{
#' # Scale an existing TikZ file down to 80%
#' tikz_inject_scale("figures/plot1.tex", scale = 0.8)
#' }
#'
#' @seealso
#' \code{\link[tikzDevice]{tikz}} for creating TikZ figures,
#' \code{\link{render_ggplot_to_latex}} for an integrated export workflow.
#'
#' @export
tikz_inject_scale <- function(path, scale) {
  lines <- readLines(path, warn = FALSE)

  i <- grep("^\\\\begin\\{tikzpicture\\}(\\[.*\\])?", lines)[1]
  if (is.na(i)) {
    return(invisible(FALSE))
  } # nothing to do

  line <- lines[i]

  if (grepl("\\[.*\\]", line)) {
    # Already has options: add or replace scale=
    if (grepl("(^|,)(\\s*)scale\\s*=", line)) {
      line <- sub("(scale\\s*=\\s*)[^,\\]]+", paste0("\\1", scale), line)
    } else {
      line <- sub("\\[(.*)\\]", paste0("[\\1, scale=", scale, "]"), line)
    }
  } else {
    # No options bracket: add one with scale only
    line <- sub(
      "^\\\\begin\\{tikzpicture\\}",
      paste0("\\\\begin{tikzpicture}[scale=", scale, "]"),
      line
    )
  }

  lines[i] <- line
  writeLines(lines, path, useBytes = TRUE)
  invisible(TRUE)
}
