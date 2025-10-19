#' Convert a LaTeX table environment (e.g., from `tabular` to `tabularx`)
#'
#' @description
#' Converts the LaTeX table environment in a captured LaTeX object or character
#' vector from `tabular` to another environment such as `tabularx`.
#' This is primarily used to ensure that tables occupy the full text width (e.g.,
#' using `tabularx` with `\\textwidth`) and to inject the
#' `@{\\extracolsep{\\fill}}` directive for even column spacing.
#'
#' The function is environment-agnostic and can be applied to any LaTeX object
#' returned by functions like `kableExtra::kbl()`, or to a raw character vector
#' of LaTeX code.
#'
#' @param x A LaTeX table, either as a character vector of LaTeX lines, or an
#'   object that prints to LaTeX (e.g., a `kableExtra` table). The content is
#'   captured using `ltx_capture_output()`.
#' @param env Character scalar specifying the target LaTeX environment to use.
#'   Defaults to `"tabularx"`. Common alternatives include `"tabular*"` or
#'   `"longtable"`.
#' @param width Character scalar giving the width argument for the environment.
#'   Defaults to `"\\textwidth"`. This is typically required for environments
#'   such as `tabularx` or `tabular*`.
#' @param print_tbl Logical; if `TRUE`, the modified LaTeX table is printed to
#'   the console and copied to the clipboard via `ltx_print_tbl()`. Default
#'   `FALSE`.
#'
#' @details
#' The function searches for the first LaTeX `\\begin{tabular}` / `\\end{tabular}`
#' pair and replaces it with a matching `\\begin{<env>}` / `\\end{<env>}` pair.
#' The column specification within the `\\begin` line is preserved but wrapped
#' with `@{\\extracolsep{\\fill}}` to distribute columns evenly across the given
#' width.
#'
#' If no `tabular` environment is found, the function issues a warning and
#' returns the input unmodified.
#'
#' This transformation is purely textual and does not reformat the table content.
#' It is typically used immediately before saving a LaTeX table to disk with
#' [`ltx_save_table()`].
#'
#' @return Invisibly returns a character vector containing the modified LaTeX
#'   lines. If `print_tbl = TRUE`, the modified table is also printed to the
#'   console and copied to the clipboard.
#'
#' @examples
#' \dontrun{
#' # Example: convert a kableExtra LaTeX table to tabularx
#' tbl <- kableExtra::kbl(head(mtcars), format = "latex", booktabs = TRUE)
#'
#' ltx_environment(tbl, env = "tabularx", width = "\\textwidth", print_tbl = TRUE)
#'
#' # Integrate with ltx_save_table
#' ltx_save_table(
#'   tbl,
#'   tbl_path = "overleaf/tables/demo_tabx.tex",
#'   preprocess = ltx_environment
#' )
#' }
#'
#' @seealso [ltx_save_table()] for saving LaTeX tables with metadata.
#' @export
ltx_environment <- function(
  x,
  env = "tabularx",
  width = "\\\\textwidth",
  print_tbl = FALSE
) {
  # Ensure character vector
  x <- ltx_capture_output(x)

  # Patterns
  begin_pat <- "\\\\begin\\{tabular\\}"
  end_pat <- "\\\\end\\{tabular\\}"

  idx_start <- stringr::str_which(x, begin_pat)
  idx_end <- stringr::str_which(x, end_pat)

  if (length(idx_start) == 0 || length(idx_end) == 0) {
    cli::cli_warn("No tabular environment found — returning unmodified input.")
    return(invisible(x))
  }

  # Replace begin environment and inject width
  x[idx_start] <- stringr::str_replace(
    x[idx_start],
    begin_pat,
    sprintf("\\\\begin\\{%s\\}\\{%s\\}", env, width)
  )

  # Extract column spec
  col_spec <- stringr::str_extract(x[idx_start], "\\{[^{}]+\\}$") |>
    stringr::str_remove_all("[{}]") |>
    trimws()

  # Insert \extracolsep after opening brace
  x[idx_start] <- stringr::str_replace(
    x[idx_start],
    paste0("\\{", col_spec, "\\}$"),
    paste0("{@{\\\\extracolsep\\{\\\\fill\\}}", col_spec, "}")
  )

  # Replace closing environment
  x[idx_end] <- stringr::str_replace(
    x[idx_end],
    end_pat,
    sprintf("\\\\end\\{%s\\}", env)
  )

  ltx_print_tbl(x, print = print_tbl)

  invisible(x)
}
