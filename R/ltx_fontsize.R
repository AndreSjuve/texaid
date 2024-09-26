#' Set Font Size for LaTeX Table
#'
#' This function modifies the LaTeX code for a table to set the font size and
#' line spacing. It appends the necessary LaTeX command to the table's LaTeX
#' code.
#'
#' @param x A character vector representing LaTeX code for a table, typically
#'          captured via `ltx_capture_output()`.
#' @param font_size Numeric value indicating the desired font size
#'                  (default is 8).
#' @param skip Numeric value for the line spacing (skip), typically set to 1.2
#'             times the font size. If `NULL`, it defaults to 1.2 * `font_size`.
#' @param print_tbl Logical, whether to print the modified LaTeX table to the
#'                  console (default is `FALSE`).
#' @param verbose Logical, whether to display informative messages via the `cli`
#'                package (default is `TRUE`).
#'
#' @return Returns the modified LaTeX code as a character vector (invisible).
#'
#' @details
#' The function appends a LaTeX `\\fontsize{}` and `\\selectfont` command
#' directly after the `\\begin{table}` tag in the input LaTeX code. If the
#' `skip` parameter is `NULL`, the function sets the skip value to 1.2 times
#' the `font_size`.
#'
#' The function validates that `font_size` and `skip` are positive values and
#' that a `\\begin{table}` tag is present. If verbose is enabled, informative
#' messages are displayed to guide the user.
#'
#' @examples
#' \dontrun{
#' # Sample LaTeX table code
#' table_code <-
#' c("\\begin{table}",
#' "\\centering",
#' "\\begin{tabular}{cc}",
#' "A & B \\\\",
#' "1 & 2 \\\\",
#' "\\end{tabular}",
#' "\\end{table}")
#'
#' # Modify font size to 10
#' modified_code <- ltx_fontsize(table_code, font_size = 10, skip = 12, print_tbl = TRUE)
#'}
#' @seealso `ltx_capture_output`
#' @export
ltx_fontsize <- function(x,
                         font_size = 8,
                         skip = NULL,
                         print_tbl = FALSE,
                         verbose = TRUE) {

  # Capture the output
  x <- ltx_capture_output(x)

  # Check for invalid font size
  if (font_size <= 0) {
    cli::cli_abort("Font size must be a positive number.")
  }

  # Check if skip is provided, otherwise default to 1.2 * font_size
  if (is.null(skip)) {
    if (verbose) {
      cli::cli_alert_info("skip argument is NULL, defaulting to 1.2 times font_size")
    }
    skip <- round(1.2 * font_size)
  }

  # Check if skip is valid
  if (skip <= 0) {
    cli::cli_abort("Skip must be a positive number.")
  }

  # Locate the beginning of the table
  tbl_start <- stringr::str_which(x, "\\\\begin\\{table\\}")

  # Ensure the table actually has a \\begin{table} tag
  if (length(tbl_start) == 0) {
    cli::cli_abort("No 'begin table' tag found in the LaTeX input.")
  }

  # Set font size command
  set_fontsize <- paste0("\\fontsize{", font_size, "}{", skip,"}\\selectfont")

  # Append the font size setting after the beginning of the table
  x <- append(x, set_fontsize, after = tbl_start)

  # Verbose messaging
  if (verbose) {
    cli::cli_alert_info("Font size set to {font_size} with skip set to {skip}")
  }

  # Print the table if requested
  ltx_print_tbl(x, print = print_tbl)

  invisible(x)
}
