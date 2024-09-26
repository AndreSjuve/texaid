#' Adjust LaTeX Table Placement
#'
#' This function modifies the placement specifier of LaTeX tables within a
#' provided text. It allows for verbose feedback during execution and the option
#'  to selectively replace specific table environments.
#'
#' @param x A character vector containing LaTeX code that includes
#'          `\begin{table}` environments.
#' @param tbl_placement A string specifying the desired table placement
#'                      (e.g., `!htbp`). If NULL, defaults to `!htbp`.
#' @param print_tbl A logical indicating whether to print the modified LaTeX
#'                  table to the console. Default is FALSE.
#' @param verbose A logical indicating whether to provide feedback during
#'                execution. Default is TRUE.
#' @param replace_idx An integer specifying which occurrence of the
#'                    `\begin{table}` environment to replace. If NULL, all
#'                    occurrences will be replaced. Default is NULL.
#'
#' @return The modified LaTeX code as a character vector, invisibly.
#'
#' @examples
#' \dontrun{
#' # Example LaTeX code with table environments
#' latex_code <- c(
#'   "\\begin{table}",
#'   "\\centering",
#'   "\\begin{tabular}{cc}",
#'   "A & B \\\\",
#'   "1 & 2 \\\\",
#'   "\\end{tabular}",
#'   "\\end{table}",
#'   "\\begin{table}",
#'   "\\begin{tabular}{cc}",
#'   "A & B \\\\",
#'   "1 & 2 \\\\",
#'   "\\end{tabular}",
#'   "\\end{table}"
#' )
#'
#' # Replace all table placements with '!htbp'
#' ltx_placement(latex_code, tbl_placement = "!htbp")
#'
#' # Replace only the first table placement with 'H'
#' ltx_placement(latex_code, tbl_placement = "H", replace_idx = 1)
#' }
#' @export
ltx_placement <- function(x,
                          tbl_placement = NULL,
                          print_tbl = FALSE,
                          verbose = TRUE,
                          replace_idx = NULL) {

  # Capture the LaTeX output
  x <- ltx_capture_output(x)

  # Set default table placement if not provided
  if (is.null(tbl_placement)) {
    tbl_placement <- "!htbp"
    if (verbose) {
      cli::cli_alert_info("No table placement provided, defaulting to '!htbp'")
    }
  }

  # Pattern to match \begin{table} (with or without placement specifier)
  search_pattern <- "\\\\begin\\{table\\}(\\[.*?\\])?"

  # Create the replacement string with the desired table placement
  replacement_string <- paste0("\\\\begin{table}[", tbl_placement, "]")

  # Identify lines containing the \begin{table} environment
  idx <- stringr::str_which(x, search_pattern)

  # Handle case when no \begin{table} environment is found
  if (length(idx) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No \\begin{table} environment found in the input.")
    }
  } else {
    # If replace_idx is specified, replace only that specific index
    if (!is.null(replace_idx)) {
      if (replace_idx > length(idx)) {
        cli::cli_alert_warning(paste("The index", replace_idx, "is out of bounds. No replacement made."))
      } else {
        if (verbose) {
          cli::cli_alert_info(paste("Replacing table environment at index", replace_idx))
        }
        x[idx[replace_idx]] <- stringr::str_replace(x[idx[replace_idx]], search_pattern, replacement_string)
      }
    } else {
      # Replace all occurrences of \begin{table} with the placement specifier
      if (verbose) {
        cli::cli_alert_info("Replacing all {length(idx)} begin table occurrences")
      }
      x[idx] <- stringr::str_replace(x[idx], search_pattern, replacement_string)
    }
  }

  # Print the modified table if requested
  ltx_print_tbl(x, print = print_tbl)

  # Return the modified table (invisible output)
  invisible(x)
}
