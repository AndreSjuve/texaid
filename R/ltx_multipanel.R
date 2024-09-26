#' Multipanel LaTeX Table Generator
#'
#' **Deprecated**: This function is deprecated and should not be used.
#' It will be removed in a future version. Please consider using alternative functions
#' for multipanel table generation.
#'
#' This function takes a list of LaTeX tabular panels, combines them, and outputs the result.
#' The bottom rule of the first panel is removed, and the second panel is resized.
#'
#' @param x A list containing two elements: two LaTeX tabular environments.
#' @param print_tbl A logical indicating whether to print the final table and copy it to the clipboard. Default is FALSE.
#'
#' @return The combined LaTeX table as a character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage (Note: this function is deprecated)
#' result <- ltx_multipanel(list(panel1, panel2))
#' }
#' @importFrom stringr str_which
ltx_multipanel <- function(x, print_tbl = FALSE) {

  warning("The `ltx_multipanel()` function is deprecated and will be removed in future versions. Please do not use it.",
          call. = FALSE)


  panel1 <- x[[1]]
  panel2 <- x[[2]]

  # Remove bottomrule from panel 1
  panel1 <- panel1[-stringr::str_which(panel1, "\\\\bottomrule")]
  idx_end_panel1 <- stringr::str_which(panel1, "\\\\end\\{tabular\\}")
  panel1_part1 <- panel1[1:idx_end_panel1]
  panel1_part2 <- panel1[(idx_end_panel1 + 1):length(panel1)]

  panel2 <- panel2[stringr::str_which(panel2, "\\\\resizebox"):stringr::str_which(panel2, "\\\\end\\{tabular\\}")]

  x <- c(panel1_part1, panel2, panel1_part2)

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }

  invisible(x)
}
