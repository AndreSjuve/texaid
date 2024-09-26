#' Deprecated: ltx_environment function
#'
#' This function is deprecated and should not be used anymore.
#' It remains available for backward compatibility.
#'
#' @param x A character vector representing LaTeX code for a table.
#' @param print_tbl Logical, whether to print the modified LaTeX table
#'                  to the console (default is `FALSE`).
#'
#' @return Returns the modified LaTeX code as a character vector (invisible).
#'
#' @details
#' This function modifies the LaTeX code by changing the table environment from
#' `tabular` to `tabularx`. However, it is deprecated and should no longer be
#' used.
#'
#' @seealso [kable_wrapper()]
#'
#' @export
#'
ltx_environment <- function(x, print_tbl = FALSE) {
  warning("The `ltx_environment()` function is deprecated and will be removed in future versions. Please do not use it.",
          call. = FALSE)

  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }

  first_search_pattern  <- "\\\\begin\\{tabular\\}"
  first_replace_pattern <- "\\\\begin\\{tabularx\\}\\{\\\\textwidth\\}"
  last_search_pattern   <- "\\\\end\\{tabular\\}"

  idx_start <- stringr::str_which(x, first_search_pattern)
  idx_end   <- stringr::str_which(x, last_search_pattern)

  f_str_alter <- stringr::str_replace(x[idx_start],
                                      first_search_pattern,
                                      first_replace_pattern)

  col_spec <- stringr::str_extract(
    stringr::str_extract_all(f_str_alter, "\\{[^{}]+\\}$")[[1]], "(\\w+\\s*)+"
  )

  new_col_spec <- paste0("\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}", col_spec, "\\}")
  x[idx_start] <- stringr::str_replace(f_str_alter, "\\{[^{}]+\\}$", new_col_spec)
  x[idx_end]   <- stringr::str_replace(x[idx_end], "tabular", "tabularx")

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }

  invisible(x)
}










#_______________________________________________________________________________
# Copyright Andre W. Sjuve 2024 ----
