#' Resize LaTeX Tabular Environments
#'
#' This function wraps LaTeX tabular-like environments
#' (e.g., `tabular`, `tabularx`, `longtable`) with a `\resizebox` command to
#' resize them to fit the `\linewidth`. The function excludes resizing for
#' `table` environments to prevent undesired behavior. This is useful for
#' ensuring tables are properly scaled within the document margins in LaTeX.
#'
#' @param x A character vector representing the LaTeX code that contains tabular
#'          environments.
#' @param print_tbl A logical flag indicating whether the modified table should
#' be printed to the console and copied to the clipboard. Defaults to `FALSE`.
#' @param verbose A logical flag to indicate whether messages should be printed
#' to the console. Defaults to `TRUE`.
#'
#' @details
#' The function works by identifying lines containing `\begin{tabular}`,
#' `\begin{tabularx}`, or other supported tabular environments
#' (excluding `table`), and wraps these environments with a
#' `\resizebox{\\linewidth}{!}{...}` command. It ensures that the
#' `\end{tabular}` line is closed properly.
#'
#' @return
#' The modified LaTeX code with the `resizebox` applied around tabular
#' environments, returned as a character vector. If `print_tbl` is `TRUE`,
#' the modified table is printed to the console.
#'
#' @examples
#' \dontrun{
#' # Example LaTeX table code
#' x <- c("\\begin{tabular}{ll}", "A & B \\\\", "\\end{tabular}")
#' ltx_resize(x)
#' }
#' @export
ltx_resize <- function(x, print_tbl = FALSE, verbose = TRUE) {

  x <- ltx_capture_output(x)

  # Generalize to capture any LaTeX environment except for 'table'
  begin_env_pattern <- "\\\\begin\\{(?!table)[^}]+\\}"
  end_env_pattern <- "\\\\end\\{(?!table)[^}]+\\}"

  # Find positions of begin and end of non-table LaTeX environments
  end_place   <- stringr::str_which(x, end_env_pattern)
  start_place <- stringr::str_which(x, begin_env_pattern)

  # Ensure both start and end places are found
  if (length(start_place) == 0 || length(end_place) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No valid LaTeX non-table environment found.")
    }
    return(invisible(NULL))
  }

  # Add a second } to the end of the environment, only if not already present
  if (!grepl("\\}\\}$", x[end_place])) {
    x[end_place] <- paste0(x[end_place], "}")
  }

  # Ensure that the resizebox is inserted correctly and not at the first line
  if (start_place > 1) {
    x <- append(x, "\\resizebox{\\linewidth}{!}{", after = start_place - 1)
  } else {
    if (verbose) {
      cli::cli_alert_warning("Cannot insert \\resizebox before the first line.")
    }
  }

  # Optionally print the table
  ltx_print_tbl(x, print = print_tbl)

  # Return the updated LaTeX code
  invisible(x)
}

