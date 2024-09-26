#' Round Numbers in LaTeX Tables
#'
#' This function takes a LaTeX table with numerical data and rounds numbers
#' based on a specified threshold. Numbers greater than or equal to the threshold
#' will be rounded to a specified number of decimal places (big_dec), while
#' numbers below the threshold will be rounded to a different number of decimal
#' places (small_dec).
#'
#' @param latex_table A character vector, data frame, or matrix representing
#'   a LaTeX table. The function will round numerical values within the LaTeX
#'   code.
#' @param threshold A numeric value that determines the cutoff for rounding.
#'   Numbers greater than or equal to this value will be rounded to `big_dec`
#'   decimal places, while those below will be rounded to `small_dec` decimal
#'   places.
#' @param big_dec An integer indicating the number of decimal places to round
#'   to for numbers greater than or equal to the threshold. Default is 0.
#' @param small_dec An integer indicating the number of decimal places to round
#'   to for numbers below the threshold. Default is 2.
#' @param print_tbl A logical flag indicating whether the modified table should
#' be printed to the console and copied to the clipboard. Defaults to `FALSE`.
#'
#' @return A character vector representing the modified LaTeX table with rounded
#'   numbers.
#' @export
#'
#' @examples
#' \dontrun{
#' latex_table <- c(
#'   "\\begin{tabular}{|c|c|c|}",
#'   "\\hline",
#'   "Value A & Value B & Value C \\\\",
#'   "\\hline",
#'   "1.234 & 10.5678 & 12345.6789 \\\\",
#'   "2.345 & 9.8765 & 6789.0123 \\\\",
#'   "\\hline",
#'   "\\end{tabular}"
#' )
#'
#' rounded_table <- ltx_round_numbers(latex_table, threshold = 10)
#' print(rounded_table)
#' }
ltx_round_numbers <- function(latex_table,
                              threshold = 10,
                              big_dec = 0,
                              small_dec = 2,
                              print_tbl = FALSE) {
  # Check that inputs are valid
  if (!is.character(latex_table) && !is.data.frame(latex_table) && !is.matrix(latex_table)) {
    stop("latex_table must be a character vector, data frame, or matrix.")
  }
  if (!is.numeric(threshold) || length(threshold) != 1) {
    stop("threshold must be a single numeric value.")
  }

  options(digits = 15)

  # Function to round numbers in the text
  round_numbers <- function(x, sdec_limit = small_dec,
                            bdec_limit = big_dec,
                            num_limit = threshold) {
    stringr::str_replace_all(
      x,
      pattern =  "(-?\\d+(,\\d+)*(\\.\\d+)?)",  # regex to match numbers
      replacement = function(match,
                             thres = num_limit,
                             bdec = bdec_limit,
                             sdec = sdec_limit) {
        num <- as.numeric(stringr::str_remove_all(match, ","))

        # Check if the conversion resulted in NA
        if (is.na(num)) {
          return(match)  # Return the original match if conversion fails
        }

        # Check if the number is greater than or equal to the threshold
        if (num >= thres) {
          rounded_num <- format(round(num, bdec), big.mark = ",")
          return(as.character(rounded_num))
        } else if (num < thres) {
          rounded_num <- format(round(num, small_dec), big.mark = ",")
          return(as.character(rounded_num))
        } else {
          return(match)
        }
      }
    )
  }

  # Convert the input to a character vector if it's a data frame or matrix
  if (is.data.frame(latex_table)) {
    # Convert each row to a single string
    latex_table <- apply(latex_table, 1, paste, collapse = " & ")
  } else if (is.matrix(latex_table)) {
    # Same for matrices
    latex_table <- apply(latex_table, 1, paste, collapse = " & ")
  } else if (!is.character(latex_table)) {
    # Just in case
    latex_table <- as.character(latex_table)
  }

  # Apply the rounding function to each line of the latex_table
  modified_table <- sapply(latex_table, round_numbers, USE.NAMES = FALSE)

  ltx_print_tbl(modified_table, print = print_tbl)
  invisible(modified_table)
}
