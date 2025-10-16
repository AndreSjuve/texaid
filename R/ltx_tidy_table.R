#' Tidy and Format LaTeX Table
#'
#' This function processes a LaTeX table by applying a series of transformations:
#' adding a caption, adjusting placement, setting row spacing, rounding numbers,
#' and wrapping numbers in math mode. It streamlines the formatting of LaTeX tables
#' to ensure consistency in presentation and compliance with LaTeX math mode formatting.
#'
#' @param x A character vector or list containing the LaTeX table input.
#' @param tbl_note A string representing the caption to be added to the table.
#' Defaults to "".
#' @param replace_idx A numeric value representing the index at which to replace
#' the table placement. Defaults to 1.
#' @param array_stretch A numeric value to set the `\\arraystretch` for row
#' spacing. Defaults to 1.2.
#' @param threshold A numeric value defining the threshold for rounding large
#' numbers. Numbers greater than or equal to this will use `big_dec`.
#' Defaults to 10.
#' @param big_dec Number of decimal places for large numbers (>= threshold).
#' Defaults to 0.
#' @param small_dec Number of decimal places for small numbers (< threshold).
#' Defaults to 2.
#' @param print_tbl Logical, if `TRUE`, the modified LaTeX table will be printed
#' to the console. Defaults to `TRUE`.
#'
#' @details
#' The function works by applying the following steps:
#' - `ltx_capture_output(x)`: Converts the input LaTeX table into a manipulable format.
#' - `ltx_caption(tbl_note)`: Adds a caption to the LaTeX table.
#' - `ltx_placement(replace_idx)`: Adjusts the LaTeX table placement.
#' - `ltx_stretch(array_stretch)`: Sets the row spacing for the table by modifying the `\\arraystretch`.
#' - `ltx_round_numbers(threshold, big_dec, small_dec)`: Rounds the numbers in the table based on the given threshold.
#' - `ltx_wrap_table_math()`: Wraps the numbers in LaTeX math mode (`$number$`) where appropriate.
#'
#' @return The processed LaTeX table is returned invisibly.
#' If `print_tbl = TRUE`, the table is printed to the console.
#'
#' @examples
#' \dontrun{
#' # Example LaTeX input
#' latex_input <- c("\\begin{table}", "\\begin{tabular}{|c|c|}", "\\hline",
#'                  "Value & 1234 \\\\", "Temperature & 98.6 \\\\", "\\hline",
#'                  "\\end{tabular}", "\\end{table}")
#'
#' # Tidy the table and print it
#' ltx_tidy_table(latex_input, tbl_note = "Sample Table", array_stretch = 1.5)
#' }
#'
#' @export
ltx_tidy_table <- function(
  x,
  tbl_path = NULL,
  tbl_note = "",
  replace_idx = 1,
  array_stretch = 1.2,
  threshold = 10,
  big_dec = 0,
  small_dec = 2,
  numbers_math_mode = FALSE,
  ruler_based = TRUE,
  print_tbl = TRUE
) {
  # Ensure x is valid LaTeX table input
  if (is.null(x) || length(x) == 0) {
    cli::cli_abort("The LaTeX table input cannot be null or empty.")
  }

  # Apply transformations in a pipeline
  processed_table <-
    ltx_capture_output(x) |>
    ltx_placement(replace_idx = replace_idx) |>
    ltx_stretch(array_stretch = array_stretch) |>
    ltx_caption(tbl_note = tbl_note)

  if (numbers_math_mode) {
    processed_table <-
      processed_table |>
      ltx_round_numbers(
        threshold = threshold,
        big_dec = big_dec,
        small_dec = small_dec
      ) |>
      ltx_wrap_table_math(ruler_based = ruler_based)
  }

  if (is.null(tbl_path)) {
    cli::cli_alert_danger(
      "Argument {.arg tbl_path} is empty. Cannot save table and print with metadata"
    )
    ltx_print_tbl(processed_table, print = print_tbl)
  } else {
    ltx_save_table(
      x = processed_table,
      tbl_path = tbl_path,
      print_tbl = print_tbl
    )
  }
  invisible(processed_table)
}
