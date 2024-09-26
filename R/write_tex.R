#' Write a TeX File
#'
#' This function writes a character vector to a .tex file. It ensures the
#' specified path has a .tex extension, checks for directory existence, and
#' allows for overwriting existing files with user feedback.
#'
#' @param x A character vector to write to the file.
#' @param path A character string specifying the file path. Should be a
#'              valid path for a .tex file.
#' @param verbose A logical value indicating whether to print messages and
#'                warnings. Default is TRUE.
#'
#' @return The function returns the input character vector invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Write a simple LaTeX document
#' write_tex(c("\\documentclass{article}", "\\begin{document}", "Hello, world!", "\\end{document}"),
#'            "output/document.tex")
#'}
#' @importFrom fs path_ext_set
#' @importFrom fs path_ext
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_subset
write_tex <- function(x, path, verbose = TRUE) {
  # Check if path is a character string
  if (!is.character(path)) {
    stop("Please specify a correct path.")
  }

  # Check if the file extension is .tex
  if (!is_tex(path)) {
    if (verbose) {
      warning("File extension is not .tex; changing extension to .tex", call. = FALSE)
    }
    path <- fs::path_ext_set(path, ext = "tex")
  }

  # Normalize the file path
  path <- normalizePath(path, mustWork = FALSE)

  # Check if the directory exists and is writable
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    if (verbose) {
      warning("Directory does not exist. Creating directory:", dir_path)
    }
    dir.create(dir_path, recursive = TRUE)
  }

  # Check if the file already exists
  if (file.exists(path)) {
    if (verbose) {
      warning("File already exists at path:", path,
              "\nOverwriting existing file.")
    }
  }

  # Write to the file
  file_conn <- file(path)
  on.exit(close(file_conn), add = TRUE)  # Ensure file connection is closed
  writeLines(x, con = file_conn)  # Using writeLines for cleaner output

  # Confirm successful write
  if (verbose) {
    message("File written successfully to:", path)
  }

  return(invisible(x))
}
