#' Save a ggplot Figure as a TikZ File
#'
#' This function saves a ggplot figure as a TikZ file for inclusion in LaTeX documents.
#' It checks for a valid ggplot object, ensures the file path is specified, and
#' appends the `.tex` extension if not provided. The function also allows sanitization
#' of special characters in the output file.
#'
#' @param fig A ggplot object. This is the figure to be saved as a TikZ file.
#' @param path A character string specifying the file path where the TikZ file will be saved.
#' If no extension is provided, `.tex` will be appended.
#' If the directory does not exist, an error will be raised.
#' @param sanitize Logical, whether to sanitize special characters in the LaTeX output.
#' Defaults to `TRUE`.
#' @param width Numeric, specifying the width of the figure in inches. Defaults to `6.4`.
#' @param height Numeric, specifying the height of the figure in inches. Defaults to `3.54`.
#'
#' @return The path to the saved TikZ file is returned invisibly.
#'
#' @importFrom tikzDevice tikz
#' @importFrom grDevices dev.off
#'
#' @details
#' This function uses the `tikzDevice` package to generate TikZ code from a ggplot object.
#' It ensures that the file path provided is valid and appends the `.tex` extension if missing.
#' If the specified directory does not exist, the function will abort with an error message.
#' Additionally, the function provides useful information to the user, such as warnings
#' when no extension is provided and confirmation once the file is saved successfully.
#'
#' @note The `tikzDevice` package must be installed to use this function.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Create a sample plot
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_minimal()
#'
#' # Save the plot as a TikZ file
#' save_tikz(fig = p, path = "my_plot")
#' }
#'
#' @export
save_tikz <- function(fig = NULL, path = NULL, sanitize = TRUE, width = 6.4, height = 3.54) {

  # Check if fig is NULL
  if (is.null(fig)) {
    cli::cli_abort("fig is NULL, provide figure to save.")
  }

  # Check if fig is a valid ggplot object
  if (!inherits(fig, "gg")) {
    cli::cli_abort("The provided figure is not a valid ggplot object.")
  }

  # Check if path is NULL
  if (is.null(path)) {
    cli::cli_abort("path is NULL, don't know where to save the figure file.")
  }

  # If the path doesn't include a folder, use the manuscript/content directory as default
  if (fs::path_file(fs::path_ext_remove(path)) == path) {
    path <- fs::path("manuscript", "content", fs::path_ext_remove(path), ext = "tex")
  }

  # If no extension is given, add .tex extension
  if (fs::path_ext(path) == "") {
    cli::cli_alert_info("No file extension given, adding .tex")
    path <- fs::path_ext_set(path, "tex")
  }

  # Ensure the directory exists
  dir_path <- fs::path_dir(path)
  if (!fs::dir_exists(dir_path)) {
    cli::cli_abort(paste0("Directory does not exist: ", dir_path))
  }

  # Start the TikZ device
  tikzDevice::tikz(file = path, width = width, height = height, sanitize = sanitize, verbose = FALSE)

  # Attempt to print the figure
  tryCatch({
    print(fig)
    grDevices::dev.off()  # Close the device after rendering
    cli::cli_alert_success("Figure is saved to: {path}")
  }, error = function(e) {
    grDevices::dev.off()  # Close the device in case of error
    cli::cli_abort(paste0("An error occurred while saving the figure: ", e$message))
  })

  invisible(path)  # Return the path invisibly
}

