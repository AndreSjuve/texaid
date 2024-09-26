#' Check and Convert Data to Tibble
#'
#' This function checks whether the input data is a data frame or a tibble.
#' If the input is neither, it attempts to convert it to a tibble.
#' It provides informative messages based on the `verbose` argument.
#'
#' @param data The data to be checked and potentially converted. It can be a
#'              data frame, tibble, or other data structures that can be
#'              converted to a tibble.
#' @param verbose A logical value indicating whether to print informational
#'                messages during the process. Defaults to TRUE.
#'
#' @return Returns the input data as a tibble if it was successfully converted,
#'         or the original data if it was already a data frame or tibble.
#'
#' @examples
#' \dontrun{
#' # Example with a data frame
#' df <- data.frame(a = 1:3, b = 4:6)
#' check_and_convert(df)  # Should return the original data frame as a tibble
#'
#' # Example with a tibble
#' tb <- tibble::tibble(a = 1:3, b = 4:6)
#' check_and_convert(tb)  # Should return the original tibble
#'
#' # Example with a numeric vector (should convert to tibble)
#' vec <- c(1, 2, 3)
#' check_and_convert(vec)  # Should return a tibble with a single column
#'
#' # Example with an incompatible structure
#' invalid_data <- list(a = 1, b = 2)
#' check_and_convert(invalid_data)  # Should raise an error
#'}
#' @import tibble
#' @importFrom cli cli_alert_info cli_abort
#' @export
check_and_convert <- function(data, verbose = TRUE) {
  # Check if data is a data frame or tibble
  if (is.data.frame(data) & verbose) {
    cli::cli_alert_info("Input is a data frame.")
  } else if (tibble::is_tibble(data) & verbose) {
    cli::cli_alert_info("Input is a tibble.")
  } else {
    # Attempt to convert to a tibble and handle any errors
    if (verbose) {
      cli::cli_alert_info("Input is neither a data frame nor a tibble. Attempting to convert to tibble.")
    }
    converted_data <- tryCatch(
      {
        tibble::as_tibble(data)  # Attempt conversion to tibble
      },
      error = function(e) {
        # Handle the error if conversion fails
        cli::cli_alert_info("Error in conversion: ", e$message)
        return(NULL)  # Return NULL or handle as needed
      }
    )

    # Check if conversion was successful
    if (is.null(converted_data)) {
      cli::cli_abort("Input cannot be converted to a tibble. Please provide a compatible structure.")
    } else {
      data <- converted_data
    }
  }

  # Return the validated and potentially converted data
  return(data)
}
