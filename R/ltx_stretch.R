#' Apply Array Stretch to a LaTeX Table
#'
#' This function modifies a LaTeX table to adjust the row stretch
#' (i.e., vertical space between rows) by appending the array stretch command
#' (`\ra{}`) after the beginning of a `tabular` or `tabularx` environment.
#' The `\ra{}` command is custom defined in my Overleaf as
#' `\newcommand{\ra}[1]{\renewcommand{\arraystretch}{#1}}`.
#'
#' @param x A character vector representing a LaTeX table or an object that can
#' be captured as LaTeX output using the `ltx_capture_output()` function.
#' @param array_stretch Numeric. The row stretch factor to apply.
#' Must be a positive number. Default is `1.2`.
#' @param verbose Logical. If `TRUE`, prints a message indicating the applied
#' array stretch. Default is `TRUE`.
#' @param print_tbl Logical. If `TRUE`, prints the modified LaTeX table to the
#' console. Default is `FALSE`.
#'
#' @return Returns the modified LaTeX table as a character vector with the
#' array stretch applied. Invisibly returns the modified table.
#'
#' @details The function searches for `\begin{table}` in the LaTeX table and
#' appends the array stretch command `\ra{}` right after. If no such environment
#' is found, an error is thrown. The function does not modify the
#' `\begin{table}` environment.
#'
#' @examples
#' \dontrun{
#' # Example LaTeX table
#' latex_table <- c("\\begin{table}",
#' "\\begin{tabular}{ccc}",
#' "\\hline", "1 & 2 & 3 \\\\",
#' "\\hline",
#' "\\end{tabular}",
#' "\\end{table}")
#' # Apply array stretch of 1.5
#' modified_table <- ltx_stretch(latex_table, array_stretch = 1.5, verbose = TRUE)
#'
#' # Print the modified table
#' ltx_stretch(latex_table, array_stretch = 1.5, print_tbl = TRUE)
#' }
#' @export

ltx_stretch <- function(x,
                        array_stretch = 1.2,
                        verbose = TRUE,
                        print_tbl = FALSE) {

  x <- ltx_capture_output(x)

  # Check for invalid font size
  # Check for valid numeric font size
  if (!is.numeric(array_stretch) || array_stretch <= 0) {
    cli::cli_abort("Array stretch must be a positive numeric value.")
  }

  # Locate the beginning of the table
  tbl_start <- stringr::str_which(x, "\\\\begin\\{table\\}")

  # Ensure the table actually has a \\begin{table} tag
  if (length(tbl_start) == 0) {
    cli::cli_abort("No 'begin table' tag found in the LaTeX input.")
  }

  # Set font size command
  set_stretch <- paste0("\\ra{", array_stretch, "}")

  # Append the array stretch command after the beginning of the table
  x <- append(x, set_stretch, after = tbl_start)

  # Verbose messaging
  if (verbose) {
    cli::cli_alert_info("Array stretch set to {array_stretch}.")
  }

  # Print the table if requested
  ltx_print_tbl(x, print = print_tbl)

  invisible(x)

}
