#' Capture and format output as LaTeX-compatible text
#'
#' The `ltx_capture_output` function captures the output of an object or
#' expression and formats it for LaTeX compatibility. This function can be used
#' to collect output that may be written over multiple lines and format it as a
#' character vector with appropriate line breaks for LaTeX output.
#'
#' @param x An object or expression whose output you wish to capture.
#' This can be a single object or multiple lines of text.
#'
#' @details
#' If `x` contains multiple elements (e.g., a vector or list), the function will
#' concatenate the elements using line breaks (`\n`) to ensure proper formatting
#' when the content is rendered as LaTeX. For single-element input, the output
#' will simply be captured and returned invisibly.
#'
#' The function relies on `utils::capture.output()` to handle capturing the
#' output of expressions and converting them into a character vector. The use of
#' `cat()` ensures that multi-line elements are appropriately formatted.
#'
#' @return
#' A character vector containing the captured output, formatted with line breaks
#' if necessary. The result is returned invisibly, so it can be used without
#' printing the output directly.
#'
#' @seealso [utils::capture.output()]
#'
#' @examples
#' \dontrun{
#' # Example with multiple lines
#' x <- c("Line 1", "Line 2", "Line 3")
#' ltx_capture_output(x)
#'
#' # Example with a single object
#' y <- "Single Line"
#' ltx_capture_output(y)
#'}

ltx_capture_output <- function(x) {
  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }
  invisible(x)
}
