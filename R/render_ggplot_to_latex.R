#' Export a ggplot Figure to LaTeX Format
#'
#' This function exports a ggplot figure to a specified path in TikZ format and
#' wraps it in a LaTeX figure environment.
#'
#' @param fig A ggplot object to be exported. Must not be NULL.
#' @param fig_path A character string specifying the path where the figure will be saved. Must not be NULL.
#' @param sanitize A logical indicating whether to sanitize the figure. Default is TRUE.
#' @param width A numeric value specifying the width of the figure. Default is 6.4.
#' @param height A numeric value specifying the height of the figure. Default is 3.54.
#' @param fig_out A character string specifying the output path for the wrapped figure. Optional.
#' @param title A character string for the figure title. Optional.
#' @param caption A character string for the figure caption. Optional.
#' @param label A character string for the figure label. Optional.
#' @param copy_fig A logical indicating whether to copy the figure to the clipboard. Default is TRUE.
#'
#' @return The original ggplot object, invisibly returned.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' render_ggplot_to_latex(fig = p, fig_path = "path/to/figure.tex",
#'                title = "Car Weight vs MPG", caption = "A scatter plot.",
#'                label = "fig:car_weight")
#' }
render_ggplot_to_latex <- function(
  fig = NULL,
  fig_path = NULL,
  sanitize = TRUE,
  width = 6.4,
  height = 3.54,
  tikz_scale = 1,
  fig_out = NULL,
  title = NULL,
  caption = NULL,
  label = NULL,
  copy_fig = TRUE
) {
  # Validate inputs
  if (is.null(fig)) {
    cli::cli_abort("fig is NULL, provide a figure to save.")
  }
  if (!inherits(fig, "gg")) {
    cli::cli_abort("The provided figure is not a valid ggplot object.")
  }
  if (is.null(fig_path)) {
    cli::cli_abort(
      "fig_path is NULL, don't know where to save the figure file."
    )
  }

  # Attempt to save the TikZ figure
  tryCatch(
    {
      save_tikz(
        fig = fig,
        path = fig_path,
        sanitize = sanitize,
        width = width,
        height = height,
        tikz_scale = tikz_scale
      )
    },
    error = function(e) {
      cli::cli_abort("Error saving TikZ figure: {e$message}")
    }
  )

  # Handle fig_out if NULL
  if (is.null(fig_out)) {
    cli::cli_alert_info(
      "`fig_out` is NULL, saving fig_out to same location as `fig_path`."
    )
    if (stringr::str_detect(fig_path, "fig_in")) {
      fig_out <- fs::path(
        fs::path_dir(fig_path),
        stringr::str_replace(
          fs::path_ext_remove(fs::path_file(fig_path)),
          "fig_in",
          "fig_out"
        ),
        ext = fs::path_ext(fig_path)
      )
    } else {
      fig_out <- fs::path(
        fs::path_dir(fig_path),
        paste0("fig_out_", fs::path_ext_remove(fs::path_file(fig_path))),
        ext = fs::path_ext(fig_path)
      )
    }
  }

  # Attempt to build the LaTeX figure environment
  tryCatch(
    {
      ltx_build_fig(
        fig_in = fig_path,
        fig_out = fig_out,
        title = title,
        caption = caption,
        label = label
      )
    },
    error = function(e) {
      cli::cli_abort("Error wrapping figure in LaTeX: {e$message}")
    }
  )

  # Collect and inject metadata
  meta <- collect_output_metadata(output_path = fig_out)
  write_metadata_block(fig_out, meta, mode = "auto")

  figure <- readLines(fig_out)

  if (copy_fig) {
    tryCatch(
      {
        utils::writeClipboard(figure)
        cli::cli_alert_success("Figure copied to clipboard.")
      },
      error = function(e) {
        cli::cli_alert_warning("Failed to copy to clipboard: {e$message}")
      }
    )
  }
  # Return the original ggplot object
  invisible(fig)
}
