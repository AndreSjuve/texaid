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
                height = height
            )
        },
        error = function(e) {
            cli::cli_abort("Error saving TikZ figure: {e$message}")
        }
    )

    # Attempt to build the LaTeX figure environment
    tryCatch(
        {
            ltx_build_fig(
                fig_in = fig_path,
                fig_out = fig_out,
                title = title,
                caption = caption,
                label = label,
                copy_fig = copy_fig
            )
        },
        error = function(e) {
            cli::cli_abort("Error wrapping figure in LaTeX: {e$message}")
        }
    )

    # Collect and inject metadata
    meta <- collect_output_metadata(output_path = fig_path)
    write_metadata_block(fig_path, meta, mode = "auto")

    # Return the original ggplot object
    invisible(fig)
}
