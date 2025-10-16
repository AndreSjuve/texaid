#' Create a LaTeX Table with Custom Specifications
#'
#' This function generates a LaTeX table using the `kableExtra` package.
#' It allows for customization of table appearance and formatting through various
#' parameters, including column specifications, captions, and labels.
#' Additionally, it can convert input data to a tibble if it is not already in
#' the appropriate format.
#'
#' @param tbl A data frame or tibble that contains the data to be displayed in
#'            the table. If NULL, an error is raised.
#' @param title A character string representing the title of the table.
#' @param lbl A character string representing the label for the table, used
#'            for referencing in LaTeX.
#' @param print_tbl A logical value indicating whether to print the table to
#'                  the console and copy it to the clipboard. Defaults to FALSE.
#' @param col_spec A character vector specifying the alignment of columns.
#'                  Options include "l" for left, "c" for center, and "r" for
#'                  right alignment. If NULL, defaults will be applied.
#' @param use_X_col A logical value indicating whether to use the "X" column
#'                   type in the tabularx environment. Defaults to TRUE.
#' @param X_col_place An integer indicating the position of the "X" column,
#'                    defaults to 1 (first column).
#' @param col_type A character string indicating the default column type for
#'                  the table, with options "l", "c", or "r". Defaults to "c".
#' @param verbose A logical value indicating whether to print messages during
#'                 the execution of the function. Defaults to TRUE.
#'
#' @return Returns the generated LaTeX table as a character string.
#'
#' @examples
#' \dontrun{
#' # Example usage with a data frame
#' df <- data.frame(A = 1:3, B = c("X", "Y", "Z"))
#' kable_wrapper(tbl = df, title = "Sample Table", lbl = "tab:sample")
#'
#' # Example with verbose turned off
#' kable_wrapper(tbl = df, title = "Sample Table", lbl = "tab:sample", verbose = FALSE)
#' }
#' @importFrom kableExtra kable
#' @import cli
#' @export

kable_wrapper <- function(
  tbl = NULL,
  title,
  lbl,
  col_names = NULL,
  print_tbl = FALSE,
  col_spec = NULL,
  use_X_col = TRUE,
  X_col_place = 1,
  col_type = "c",
  verbose = TRUE
) {
  if (is.null(tbl)) {
    cli::cli_abort("No table given")
  } else {
    tbl <- check_and_convert(tbl, verbose = verbose)
  }

  n_cols <- ncol(tbl)

  # Validate column type
  if (!col_type %in% c("l", "c", "r")) {
    if (verbose) {
      # Check if verbose is TRUE
      cli::cli_alert_warning("col_type not l, c or r, defaulting to c")
    }
    col_type <- "c"
  }

  if (is.null(col_spec)) {
    if (verbose) {
      # Check if verbose is TRUE
      cli::cli_alert_info("No column specification given")
    }
    if (use_X_col) {
      if (verbose) {
        # Check if verbose is TRUE
        cli::cli_alert_info(
          "Table will use X column type for tabularx environment"
        )
      }
      if (X_col_place == 1) {
        col_spec <- c("X", rep(col_type, n_cols - 1))
      } else {
        col_spec <- c(
          rep(col_type, X_col_place - 1),
          "X",
          rep(col_type, n_cols - X_col_place)
        )
      }
    } else {
      if (verbose) {
        # Check if verbose is TRUE
        cli::cli_alert_info("Table will not use X column type")
      }
      col_spec <- c("l", rep(col_type, n_cols - 1))
    }
  }

  if (is.null(col_names)) {
    cli::cli_alert_info(
      "Argument {.arg col_names} is NULL, defaulting to tibble column names."
    )
    col_names <- colnames(tbl)
  }

  if (length(col_spec) != n_cols) {
    cli::cli_abort(
      "Length of column specification does not match the number of columns in the tbl data frame"
    )
  }

  out_tbl <- kableExtra::kable(
    x = tbl,
    format = "latex",
    caption = title,
    label = lbl,
    align = col_spec,
    col.names = col_names,
    tabular = "tabularx",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    format.args = list(big.mark = ","),
    digits = 2,
    linesep = ""
  )

  ltx_print_tbl(out_tbl, print = print_tbl)
  invisible(out_tbl)
}
