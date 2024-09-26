#' Print and Copy LaTeX Table to Clipboard
#'
#' The `ltx_print_tbl` function prints a LaTeX table or text to the console and
#' copies the content to the system clipboard. This is useful for quickly
#' copying and pasting LaTeX code into a document while also optionally printing
#' it to the console.
#'
#' @param x A character string representing LaTeX table or text that you wish
#' to print and copy to the clipboard.
#' @param print Logical. If `TRUE`, the LaTeX table or text in `x` will be
#' printed to the console. Defaults to `FALSE`.
#'
#' @details
#' The function is designed to streamline the process of working with LaTeX
#' tables or other formatted text. When `print` is set to `TRUE`, the function
#' prints the content of `x` to the console with line breaks around the table
#' or text for visual clarity. It also copies the content to the system
#' clipboard using `utils::writeClipboard()`, making it easy to paste directly
#' into LaTeX documents.
#'
#' @return
#' The function does not return any value. It performs the side effects of
#' printing to the console (if `print` is `TRUE`) and copying `x` to the
#' clipboard.
#'
#' @seealso [utils::writeClipboard()]
#'
#' @examples
#' \dontrun{
#' # Example LaTeX table string
#' tbl <- "\\begin{tabular}{|c|c|}\n\\hline\nA & B \\\\\n\\hline\n1 & 2 \\\\\n\\hline\n\\end{tabular}"
#'
#' # Print and copy the table to clipboard
#' ltx_print_tbl(tbl, print = TRUE)
#' }

ltx_print_tbl <- function(x, print = FALSE) {
  if (print) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }
}
