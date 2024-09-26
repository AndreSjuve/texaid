#' Wrap Numbers in LaTeX Table in Math Mode within Tabular Environments
#'
#' This function takes a LaTeX table as input and wraps all numbers, both
#' positive and negative, in LaTeX math mode (`$number$`) only if they are located
#' within a `tabular` or `tabularx` environment. Numbers that are already
#' wrapped in math mode are ignored.
#'
#' The function handles both integers and decimal numbers, optionally
#' including commas as thousand separators and supports scientific notation.
#' It also ensures that numbers followed by superscripts (e.g., `10^{2}`)
#' are fully wrapped in math mode.
#'
#' @details
#' The function identifies numbers inside `tabular` or `tabularx` environments
#' and wraps them in dollar signs for LaTeX math mode. It uses a regular expression
#' to find numbers and ignores those that are already wrapped in math mode.
#'
#' Explanation of Regular Expression:
#'
#' `(?<!\\$)-?\\b(\\d{1,3}(,\\d{3})*(\\.\\d+)?(e-?\\d+)?)(\\^\\{[^\\}]+\\}\\S*)?(\\S*\\b)?(?![^$]*\\$)`
#'
#' 1. `(?<!\\$)`
#'    - A negative lookbehind that ensures there is no dollar sign (`$`) before the current position.
#'    - Prevents matching numbers already enclosed in LaTeX math mode (`$...$`).
#'
#' 2.  `-?`
#'     - Optional negative sign.
#'
#' 3. `\\b`
#'    - A word boundary, ensuring that the number is not part of a larger word.
#'    - Matches standalone numbers.
#'
#' 4. `(-?\\d{1,3}(,\\d{3})*(\\.\\d+)?(e-?\\d+)?)`
#'    - Matches numbers in the format:
#'      - `\\d{1,3}` : First group of digits.
#'      - `(,\\d{3})*` : Thousands groups separated by commas.
#'      - `(\\.\\d+)?` : Optional decimal numbers.
#'      - `(e-?\\d+)?` : Optional scientific notation.
#'
#' 5. `(\\^\\{[^\\}]+\\}\\S*)?`
#'    - Matches LaTeX superscript notation (e.g., `10^{2}`).
#'
#' 6. `(\\S*\\b)?`
#'    - Optionally matches any trailing characters after the number.
#'
#' 7. `(?![^$]*\\$)`
#'    - A negative lookahead to ensure there's no dollar sign ahead in the string
#'      (prevents matching numbers already enclosed in LaTeX math mode).
#'
#' @param latex_table A character vector or list containing the LaTeX table
#' input.
#' @param print_tbl A logical value indicating whether to print the table to
#' the console after modification. Defaults to `FALSE`.
#'
#' @return A modified LaTeX table where all numbers within `tabular` environments
#' are wrapped in math mode. The result is returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Example LaTeX input with numbers
#' latex_input <- c("\\begin{tabular}{|c|c|}",
#'                  "Value: 1234",
#'                  "Temperature: 98.6",
#'                  "Negative: -567.89",
#'                  "\\end{tabular}")
#'
#' # Wrap all numbers in math mode
#' result <- ltx_wrap_table_math(latex_input)
#'
#' # Output:
#' # "\\begin{tabular}{|c|c|}"
#' # "Value: $1234$"
#' # "Temperature: $98.6$"
#' # "Negative: $-567.89$"
#' # "\\end{tabular}"
#' }
#' @export
ltx_wrap_table_math <- function(latex_table, print_tbl = FALSE) {
  # Capture LaTeX table output
  latex_table <- ltx_capture_output(latex_table)

  # Find the indices of tabular environments
  stbl <- stringr::str_which(latex_table, "\\\\begin\\{(tabular|tabularx)\\}")
  etbl <- stringr::str_which(latex_table, "\\\\end\\{(tabular|tabularx)\\}")

  # Check if tabular environments were found
  if (length(stbl) == 0 || length(etbl) == 0) {
    cli::cli_abort("No valid tabular environment found.")
    return(invisible(latex_table))
  }

  # Ensure matching start and end indices for each tabular block
  if (length(stbl) != length(etbl)) {
    cli::cli_abort("Mismatch between start and end of tabular environments.")
    return(invisible(latex_table))
  }

  # Define a pattern to match numbers, excluding those already wrapped in math mode
  pattern <- "(?<!\\$)-?\\b(\\d{1,3}(,\\d{3})*(\\.\\d+)?(e-?\\d+)?)(\\^\\{[^\\}]+\\}\\S*)?(\\S*\\b)?(?![^$]*\\$)"

  # Function to wrap numbers in math mode
  wrap_in_math_mode <- function(x, num_pattern = pattern) {
    stringr::str_replace_all(x, num_pattern, replacement = \(x) paste0("$", x, "$"))
  }

  # Iterate over each tabular environment and wrap numbers in math mode
  modified_table <- latex_table
  for (i in seq_along(stbl)) {
    modified_table[stbl[i]:etbl[i]] <- sapply(latex_table[stbl[i]:etbl[i]],
                                              wrap_in_math_mode,
                                              USE.NAMES = FALSE)
  }

  # Print the modified table if requested
  ltx_print_tbl(modified_table, print = print_tbl)

  # Return the modified table invisibly
  invisible(modified_table)
}
