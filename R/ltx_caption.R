#' Process LaTeX Table Header with Caption, Label, and Note
#'
#' The `ltx_caption` function processes LaTeX table code by modifying the header,
#' including the caption, label, and table notes. It removes unnecessary lines,
#' captures the table title, and optionally appends a label and table note,
#' while also printing the table if specified.
#'
#' @param x A character vector representing the LaTeX table code
#'          that needs to be processed.
#' @param tbl_note A character string for an optional table note that will be
#'                 added to the table header.
#' @param print_tbl Logical. If `TRUE`, the processed LaTeX table will be
#'                  printed to the console. Defaults to `FALSE`.
#'
#' @details
#' This function refines LaTeX table code by removing unwanted `\\usepackage`
#' commands and blank lines, capturing and modifying the `\\caption` and
#' `\\label`, and optionally adding a custom table note. The function
#' identifies the LaTeX table title and reformats it into a custom format.
#' If a table label is present in the title, it is extracted and repositioned
#' before the table content. If a table note is provided, it is
#' appended after the caption.
#'
#' @return
#' The function returns the processed LaTeX table code as an invisible
#' character vector. If `print_tbl` is set to `TRUE`, it also prints the
#' modified table code to the console and copies it to the clipboard for easy
#' pasting into a latex editor.
#'
#' @importFrom stringr str_remove_all str_subset
#' @importFrom stringi stri_isempty
#' @importFrom utils capture.output
#' @seealso [ltx_print_tbl()], [ltx_capture_output()]
#'
#' @examples
#' \dontrun{
#' # Example LaTeX table with caption, label, and table note
#' tbl_code <- c("\\usepackage{booktabs}",
#'               "\\caption{Sample Table}",
#'               "\\label{tab:example}",
#'               "\\begin{tabular}{|c|c|}",
#'               "\\hline",
#'               "A & B \\\\",
#'               "\\hline",
#'               "1 & 2 \\\\",
#'               "\\hline",
#'               "\\end{tabular}")
#'
#' # Process the table header with a table note and print the table
#' ltx_caption(tbl_code, tbl_note = "Table Note", print_tbl = TRUE)
#'}
#' @export
ltx_caption <- function(x, tbl_note = "", print_tbl = FALSE) {

  # Capture the LaTeX output, converting it to a single string if needed
  x <- ltx_capture_output(x)

  # Define patterns to remove unnecessary lines

  # Pattern to identify LaTeX package lines
  pkg_rm_pattern <- "(\\\\usepackage\\{\\w+\\})"
  # Identify blank lines
  blank_rm_idx   <- which(stringi::stri_isempty(x))
  # Identify package lines
  pkg_rm_idx     <- stringr::str_which(x, pkg_rm_pattern)

  # Remove blank and package lines
  if (length(pkg_rm_idx) != 0) {
    x <- x[-pkg_rm_idx]
  }

  if (length(blank_rm_idx) != 0) {
    x <- x[-blank_rm_idx]
  }

  # Define patterns for LaTeX caption and label

  # Pattern for table caption
  title_pattern <- "\\\\caption\\{"
  # Pattern for table label
  label_pattern <- "\\\\label\\{[^}]+\\}"

  # Extract and clean the table title

  # Index of the title line
  title_idx     <- stringr::str_which(x, title_pattern)
  # Remove caption markers
  title_line    <- stringr::str_remove_all(stringr::str_subset(x, title_pattern),
                                           title_pattern)

  # If a label is present in the title, extract it
  if (stringr::str_detect(title_line, label_pattern)) {
    # Remove the label and closing bracket
    title <- gsub("\\\\label\\{[^}]+\\}|\\}$", "", title_line)
    # Extract the label
    label <- stringr::str_extract(title_line, label_pattern)
    # Locate the tabular environment
    tabular_idx <- stringr::str_which(x, "\\\\begin\\{(tabular|tabularx)\\}")
    # Insert the label before the table starts
    x <- append(x, label, after = (tabular_idx - 1))
  } else {

    # Clean the title
    title <- stringr::str_remove_all(
      stringr::str_subset(x, title_pattern),
      "(\\\\caption\\{|\\})")
  }

  # Add the table note if provided
  if (!stringi::stri_isempty(tbl_note)) {

    # Format the title with a header
    title_replacement <- paste0("\\caption{", "\\header{", title, "} \\\\")

    # Replace the original title
    x[title_idx] <- title_replacement

    # Append the table note
    x <- append(x, paste0(tbl_note, "}"), after = title_idx)

  } else {

    # Format the title without a table note
    title_replacement <- paste0("\\caption{", "\\header{", title, "}}")

    # Replace the original title
    x[title_idx] <- title_replacement
  }

  # Print the table and copy to clipboard if the print option is set to TRUE
  ltx_print_tbl(x, print = print_tbl)

  # Return the modified table code invisibly
  invisible(x)
}
