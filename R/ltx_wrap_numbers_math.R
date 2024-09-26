#' Wrap Numbers in LaTeX Table in Math Mode
#'
#' This function takes a LaTeX table as input and wraps all numbers, both
#' positive and negative, in LaTeX math mode (`$number$`). The function ensures
#' that numbers are not already wrapped in math mode by checking for existing dollar signs.
#'
#' The function supports both integers and decimal numbers, optionally
#' including commas as thousand separators. Additionally, it can handle scientific notation
#' (e.g., `1.23e4`) and superscripts (e.g., `10^{2}`).
#'
#' @details
#' The function uses a regular expression to identify and wrap numbers in LaTeX math mode. It:
#' 1. Ensures that numbers are not already wrapped in dollar signs.
#' 2. Supports numbers with commas as thousand separators, decimal points, and scientific notation.
#' 3. Optionally wraps numbers followed by LaTeX superscripts in math mode as well.
#'
#' Explanation of Regular Expression:
#'
#' `(?<!\\$)-?\\b(\\d{1,3}(,\\d{3})*(\\.\\d+)?(e-?\\d+)?)(\\^\\{[^\\}]+\\}\\S*)?(\\S*\\b)?(?![^$]*\\$)`
#'
#' 1. `(?<!\\$)`
#'    - A negative lookbehind that ensures no dollar sign (`$`) precedes the current position.
#'    - This prevents matching numbers already enclosed in LaTeX math mode.
#'
#' 2. `-?`
#'    - Matches an optional negative sign, allowing the pattern to capture negative numbers.
#'
#' 3. `\\b`
#'    - A word boundary to ensure the number is a standalone entity.
#'
#' 4. `(\\d{1,3}(,\\d{3})*)`
#'    - Matches numbers, potentially separated by commas as thousand separators.
#'
#' 5. `(\\.\\d+)?`
#'    - Optionally matches decimal numbers.
#'
#' 6. `(e-?\\d+)?`
#'    - Optionally matches numbers in scientific notation (e.g., `1.23e4`).
#'
#' 7. `(\\^\\{[^\\}]+\\}\\S*)?`
#'    - Optionally matches LaTeX superscripts.
#'
#' 8. `(?![^$]*\\$)`
#'    - A negative lookahead that ensures no dollar sign (`$`) follows the matched number.
#'
#' @param latex_table A character vector or list containing the LaTeX table input.
#' @param print_tbl A logical value indicating whether to print the table to the console after modification. Defaults to `FALSE`.
#'
#' @return A modified LaTeX table where all numbers are wrapped in math mode. The result is returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Example LaTeX input with numbers
#' latex_input <- c("Value: 1234", "Temperature: 98.6", "Negative: -567.89")
#'
#' # Wrap all numbers in math mode
#' result <- ltx_wrap_numbers_math(latex_input)
#'
#' # Output: "Value: $1234$", "Temperature: $98.6$", "Negative: $-567.89$"
#' }
#' @export
ltx_wrap_numbers_math <- function(latex_table, print_tbl = FALSE) {

  latex_table <- ltx_capture_output(latex_table)

  # Define a regular expression pattern to match numbers, including scientific notation
  pattern <- "(?<!\\$)-?\\b(\\d{1,3}(,\\d{3})*(\\.\\d+)?(e-?\\d+)?)(\\^\\{[^\\}]+\\}\\S*)?(\\S*\\b)?(?![^$]*\\$)"

  # Function to wrap numbers in math mode
  wrap_in_math_mode <- function(x, num_pattern = pattern) {
    stringr::str_replace_all(x, num_pattern, replacement = \(x) paste0("$", x, "$"))
  }

  # Apply the function to each line of the LaTeX table
  modified_table <- sapply(latex_table, wrap_in_math_mode)

  # Print the table if required
  ltx_print_tbl(modified_table, print = print_tbl)

  # Return the modified table invisibly
  invisible(modified_table)
}
