
# is_tex() ---------------------------------------------------------------------

#' Check if file path has tex extension
#'
#' \code{is_tex} is a function that checks if a file path has a tex extension
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param path Character vector with path
#'
#' @return TRUE if extension is tex and FALSE if not
#'
#' @export

is_tex <- function(path) {

  if (!is.character(path))
    stop("Please specify a correct path.")

  fs::path_ext(path) == "tex"
}

# ltx_caption() ----------------------------------------------------------------

#' Customise latex output
#'
#' \code{ltx_caption} is a function that modifies latex output from
#' packages such as texreg, kable and stargazer. What it does is that it removes
#' the usepackage commands added to the output if options such as booktabs =
#' TRUE is used (in texreg package). Furthermore, it changes the format of the
#' title and adds the option of including a table text directly below the main
#' title of the table.
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Output from \code{texreg::texreg}, \code{kableExtra::kable},
#'   \code{stargazer::stargazer} etc.
#' @param tbl_note   Character string with explanatory text for the table. This
#'   is placed under the table title.
#'
#' @param print_tbl TRUE/FALSE. Should the table be printed to console?
#'
#' @return This function prints out the modified latex code
#'
#' @importFrom stringr str_remove_all str_subset
#' @importFrom purrr prepend
#' @importFrom stringi stri_isempty
#' @importFrom utils capture.output
#'
#' @export

ltx_caption <- function(x, tbl_note = "", print_tbl = FALSE) {

  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }

  # Operation 1: Remove usepackages that is included when setting
  # booktabs and dcolum to TRUE. Also remove blank lines at the beginning

  pkg_rm_pattern <- "(\\\\usepackage\\{\\w+\\})"
  blank_rm_idx   <- which(stringi::stri_isempty(x))
  pkg_rm_idx     <- stringr::str_which(x, pkg_rm_pattern)

  x <- x[-c(blank_rm_idx,pkg_rm_idx)]

  # Operation 2: Fix the caption such that we get it on the following:
  # Table XX: (normal size)
  # Title (large)

  title_pattern <- "\\\\caption\\{"
  label_pattern <- "\\\\label\\{[:graph:]*\\}"
  title_idx     <- stringr::str_which(x, title_pattern)

  # Extract original title

  title_line <-
    stringr::str_remove_all(str_subset(x, title_pattern),
                            title_pattern)

  # Have to check if label{} is in title line (kabelExtra puts the label in
  # there)

  if (stringr::str_detect(title_line, label_pattern)) {

    title <-
      str_remove_all(title_line, "(\\\\label\\{[:graph:]*\\}|\\})")

    # Extract label
    label <-
      str_extract(title_line, label_pattern)

    # Position of where to insert label line
    tabular_idx <- str_which(x, "\\\\begin\\{(tabular|tabularx)\\}")

    # Insert label line
    x <- purrr::prepend(x, label, before = tabular_idx)

  } else {

    title <-
      stringr::str_remove_all(str_subset(x, title_pattern),
                              "(\\\\caption\\{|\\})")
  }

  # Create replacement

  title_replacement <-
    paste0("\\caption{", "\\\\ ", "\\large{\\textbf{",
           title, "}}}")

  # Substitute old latex caption code with new.

  x[title_idx] <- title_replacement

  # Operation 3: Add captionsetup and caption for table explanation.
  # This means that we have to insert custom latex code and allow for a tbl note
  # argument in the function

  if (!stringi::stri_isempty(tbl_note)) {

    tbl_note_setup <-
      "\\captionsetup{font = footnotesize, justification = justified, width = \\linewidth}"

    tbl_note <-
      paste0("\\caption*{", tbl_note, "}")

    x <- purrr::prepend(x, tbl_note_setup, before = title_idx + 1)
    x <- purrr::prepend(x, tbl_note, before = title_idx + 2)
  }

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }
  invisible(x)

}

# ltx_decimals() -----------------------------------------------------------

#' Remove decimals from latex code
#'
#' \code{ltx_decimals} is a function that removes decimals for numbers
#' in latex code that are bigger than n_digits. For instance if n_digits = 2,
#' and in the latex code there is a number, 23.34, this function will turn it
#' into 23.
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Latex code either in form of string or output from other functions
#'
#' @param n_digits When to remove decimals, e.g., n_digits = 3 means for numbers
#'   greater than 99.
#'
#' @param print_tbl TRUE/FALSE. Should the table be printed to console?
#'
#' @return Returns the input \code{x} with decimals removed. It prints out the
#'   output to console as well as return it invisibly for easy use in pipes.
#' @export

ltx_decimals <- function(x, n_digits = 3, print_tbl = FALSE) {

  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }

  x <-
    purrr::map_chr(x, ~str_remove_decimals_all(.x, n_digits = n_digits))

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }

  invisible(x)
}


# ltx_environment() ------------------------------------------------------------

#' Change default table environment for latex tables
#'
#' \code{ltx_environment} is a function designed to take output from
#' stargazer or kable and change the table environment from *tabular* to
#' *tabularx*, and also specify that the table should fill the entire textwidth.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param x Character vector (ouput from stargazer/kable) - latex table code
#'
#' @param print_tbl TRUE/FALSE. Should the table be printed to console?
#'
#' @return Returns latex code with a new table environment
#'
#' @export
#'
#' @importFrom stringr str_extract str_extract_all

ltx_environment <- function(x, print_tbl = FALSE) {

  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }

  first_search_pattern  <- "\\\\begin\\{tabular\\}"
  first_replace_pattern <- "\\\\begin\\{tabularx\\}\\{\\\\textwidth\\}"
  last_search_pattern   <- "\\\\end\\{tabular\\}"

  idx_start <- stringr::str_which(x, first_search_pattern)
  idx_end   <- stringr::str_which(x, last_search_pattern)

  f_str_alter <-
    stringr::str_replace(x[idx_start],
                         first_search_pattern,
                         first_replace_pattern)

  col_spec <-
    stringr::str_extract(
      stringr::str_extract_all(
        f_str_alter, "\\{[^{}]+\\}$")[[1]], "(\\w+\\s*)+")

  new_col_spec <-
    paste0("\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}", col_spec, "\\}")

  x[idx_start] <- stringr::str_replace(f_str_alter,
                                       "\\{[^{}]+\\}$",
                                       new_col_spec)
  x[idx_end]   <- stringr::str_replace(x[idx_end],
                                       "tabular",
                                       "tabularx")

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }
  invisible(x)
}

# ltx_fontsize() -----------------------------------------------------------

#' Set table fontsize
#'
#' \code{ltx_fontsize} adds the latex command fontsize{}{} to the latex
#' table allowing one to adjust the fontsize.
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Character string of latex code
#'
#' @param font_size Numeric, fontsize in latex unit *pt*
#'
#' @param print_tbl TRUE/FALSE. Should the table be printed to console?
#'
#' @return Returns a character string of latex code.
#' @export

ltx_fontsize <- function(x, font_size = 8, print_tbl = FALSE) {

  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }

  skip <- round(1.2 * font_size)

  resizebox_pattern <- "\\\\resizebox\\{\\\\linewidth\\}\\{!\\}\\{"

  has_resizebox <- any(stringr::str_detect(x, resizebox_pattern))

  if (has_resizebox) {
    start_place <- stringr::str_which(x, resizebox_pattern) + 1
  } else {
    start_place <- stringr::str_which(x, "^\\\\begin\\{(tabular|tabularx)\\}")
  }

  set_fontsize <- paste0("\\fontsize{",
                         font_size,
                         "}{",
                         skip,
                         "}\\selectfont")

  x <- purrr::prepend(x,
                      set_fontsize,
                      before = start_place)

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }

  invisible(x)


}

# ltx_multipanel() -------------------------------------------------------------

#' Create two-panel regression table
#'
#' \code{ltx_multipanel} is a function that takes a list \code{x} of two
#' latex tbls and merges them together such that they form a two-panel table.
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param x List of regression table latex code (output from texreg)
#' @param print_tbl TRUE/FALSE. Should latex code be printed to console?
#'
#' @return Latex code for multipanel table
#' @export

ltx_multipanel <- function(x, print_tbl = FALSE) {

  panel1 <- x[[1]]
  panel2 <- x[[2]]

  # Remove bottomrule from panel I

  panel1 <- panel1[-str_which(panel1, "\\\\bottomrule")]


  idx_end_panel1 <- str_which(panel1, "\\\\end\\{tabular\\}")

  panel1_part1 <- panel1[1:idx_end_panel1]

  panel1_part2 <- panel1[(idx_end_panel1 + 1):length(panel1)]

  panel2 <-
    panel2[str_which(panel2, "\\\\resizebox"):str_which(panel2,
                                                        "\\\\end\\{tabular\\}")]


  x <-
    c(panel1_part1, panel2, panel1_part2)


  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }
  invisible(x)
}


# ltx_placement() ----------------------------------------------------------

#' Set table placement for Latex table
#'
#' \code{ltx_placement} lets the user determine the placement of a latex
#' table. The options from kableExtra is limited and this gives more control.
#' The option of hold_position in \code{kable_styling} produces a warning in
#' Overleaf that the requirement is to strong.
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x  Character string with latex code
#' @param tbl_placement Character specifying table placement. Must conform to
#'   standard options in latex
#'
#' @param print_tbl TRUE/FALSE. Should the table be printed to console?
#'
#' @return Character string with latex code. Prints it out to console and
#'   returns the text in strings to use with pipes.
#' @export

ltx_placement <- function(x, tbl_placement = NULL, print_tbl = FALSE) {

  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }

  if (is.null(tbl_placement)) {
    tbl_placement <- "!ht"
  }

  search_pattern <- "\\\\begin\\{table\\}"

  replacement_string <- paste0("\\begin{table}","[", tbl_placement, "]")

  idx <- stringr::str_which(x, search_pattern)

  x[idx] <- replacement_string

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }

  invisible(x)

}


# ltx_resize() -----------------------------------------------------------------

#' Add resizebox to latex table
#'
#' \code{ltx_resize} is a function that adds the resizebox command to
#' latex table code in order to size tables to fit a page. This function is
#' mostly intended for use with output from regression tables (texreg package)
#' as this does not have an option to resize the table using resizebox.
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Character string of latex code
#'
#' @param print_tbl TRUE/FALSE. Should the table be printed to console?
#'
#' @return Returns a character string of latex code.
#' @export

ltx_resize <- function(x, print_tbl = FALSE) {

  if (length(x) > 1) {
    x <- utils::capture.output(cat(x, sep = "\n"))
  } else {
    x <- utils::capture.output(x)
  }

  # Add } to end{tabular}

  end_place <- stringr::str_which(x, "\\\\end\\{(tabular|tabularx)\\}")

  x[end_place] <- paste0(x[end_place], "}")

  # Add \resizebox{\linewidth}{!}{ before begin tabular

  start_place <- stringr::str_which(x, "^\\\\begin\\{(tabular|tabularx)\\}")

  x <- purrr::prepend(x,
                      "\\resizebox{\\linewidth}{!}{",
                      before = start_place)

  if (print_tbl) {
    cat(c("", "", "", x, "", "", ""), sep = "\n")
    utils::writeClipboard(x)
  }

  invisible(x)

}



# write_tex() ------------------------------------------------------------------

#' Write latex code to tex file
#'
#' \code{write_tex} is a function designed to take output from stargazer or
#' other functions that produce latex code in a character vector and write
#' it to a tex file.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param x object to write to tex
#'
#' @param path Character vector with path to file
#'
#' @return \code{write_tex} returns the latex code invisibly (so it can be used
#'   in a pipeline). It also writes x to a tex file.
#'
#' @export
#'
#' @importFrom fs path_ext_set

write_tex <- function(x, path) {

  if (!is.character(path))
    stop("Please specify a correct path.")

  if (!is_tex(path)) {
    warning("File extension is not tex, extension is changed",
            call. = F)

    path <- fs::path_ext_set(path, ext = "tex")
  }

  path <- normalizePath(path, mustWork = FALSE)

  file_conn <- file(path)
  cat(x, file = file_conn, sep = "\n")
  close(file_conn)

  return(invisible(x))
}








