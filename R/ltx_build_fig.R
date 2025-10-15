#' Build a LaTeX Figure Environment with a TikZ Figure
#'
#' This function wraps a TikZ figure from a provided TeX file in a LaTeX figure
#' environment. It allows for specifying the output file name, title, caption, and
#' label. It also offers the option to copy the figure to the clipboard.
#'
#' @param fig_in A character string specifying the input file path for the TikZ figure.
#'   Must not be NULL and must exist in the file system.
#' @param fig_out A character string specifying the output file path for the wrapped
#'   LaTeX figure. If NULL, it will save to the same directory as `fig_in` with a
#'   modified name.
#' @param title A character string for the figure title. Default is "Example figure".
#' @param caption A character string for the figure caption. Default is
#'   "This figure shows nothing of importance, it is work under construction.".
#' @param label A character string for the figure label. Default is "example".
#' @param copy_fig A logical indicating whether to copy the figure content to the clipboard.
#'   Default is TRUE.
#'
#' @return A character vector representing the complete LaTeX figure environment,
#'   invisibly returned.
#' @export
#'
#' @examples
#' \dontrun{
#' ltx_build_fig("path/to/tikz_figure.tex", "path/to/wrapped_figure.tex",
#'                title = "Sample Figure", caption = "This is a sample caption.",
#'                label = "sample_fig", copy_fig = TRUE)
#' }
ltx_build_fig <- function(
  fig_in = NULL,
  fig_out = NULL,
  title = NULL,
  caption = NULL,
  label = NULL
) {
  # Check for fig_in
  if (is.null(fig_in)) {
    cli::cli_abort("fig_in must be supplied. Argument is NULL.")
  }
  if (!fs::file_exists(fig_in)) {
    cli::cli_abort("fig_in not found. File does not exist, check directory.")
  }

  # Handle fig_out if NULL
  if (is.null(fig_out)) {
    cli::cli_alert_info(
      "`fig_out` is NULL, saving fig_out to same location as `fig_in`."
    )
    if (stringr::str_detect(fig_in, "fig_in")) {
      fig_out <- fs::path(
        fs::path_dir(fig_in),
        stringr::str_replace(
          fs::path_ext_remove(fs::path_file(fig_in)),
          "fig_in",
          "fig_out"
        ),
        ext = fs::path_ext(fig_in)
      )
    } else {
      fig_out <- fs::path(
        fs::path_dir(fig_in),
        paste0("fig_out_", fs::path_ext_remove(fs::path_file(fig_in))),
        ext = fs::path_ext(fig_in)
      )
    }
  }

  # Handle missing title, caption, and label with defaults
  title <- title %||% "Example figure"
  caption <- caption %||%
    "This figure shows nothing of importance, it is work under construction."
  label <- label %||% "example"

  # Ensure the label is unique and properly formatted
  label <- paste0("fig:", label)

  # Read the figure content from fig_in
  fig_body <- tryCatch(
    {
      readLines(fig_in)
    },
    error = function(e) {
      cli::cli_abort("Error reading fig_in: {e$message}")
    }
  )

  # Build LaTeX figure environment components
  fig_start <- c("\\begin{figure}[htbp!]", "    \\centering")
  fig_title <- paste0("    \\caption{\\header{", title, "} \\\\")
  fig_caption <- paste0("     ", caption, "}")
  fig_label <- paste0("    \\label{", label, "}")
  fig_end <- "\\end{figure}"

  # Assemble complete figure environment
  figure <- c(fig_start, fig_title, fig_caption, fig_label, fig_body, fig_end)

  # Write the figure to fig_out using the write_tex function
  tryCatch(
    {
      write_tex(x = figure, path = fig_out, verbose = FALSE)
      cli::cli_alert_success("Figure is saved to: {fig_out}")
    },
    error = function(e) {
      cli::cli_abort("Error writing figure to file: {e$message}")
    }
  )

  invisible(figure)
}
