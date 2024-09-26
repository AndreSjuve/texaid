#' Check if the File is a TeX File
#'
#' This function checks whether the specified file path has a `.tex` file
#' extension.
#'
#' @param path A character string specifying the file path.
#' @return A logical value (`TRUE` or `FALSE`) indicating whether the file has
#'         a `.tex` extension.
#' @examples
#' \dontrun{
#' is_tex("document.tex")  # Returns TRUE
#' is_tex("image.png")     # Returns FALSE
#' }
#' @export
#' @importFrom fs path_ext
is_tex <- function(path) {
  if (!is.character(path))
    # Error handling if the input is not a character string
    stop("Please specify a correct path.")
  # Check if the file extension is 'tex'
  fs::path_ext(path) == "tex"
}
