
# capture_stargazer_tab() ------------------------------------------------------

#' Capture everything within the tabular environment of stargazer output
#'
#' \code{capture_stargazer_tab} is a function that extracts everything inside
#' the tabular environment of stargazer output.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param star_tbl Stargazer object
#'
#' @return Character vector
#'
#' @importFrom stringr str_which

capture_stargazer_tab <- function(star_tbl) {

  tabular_lim <- stringr::str_which(star_tbl, "\\{tabular\\}")

  if (length(tabular_lim) != 2 ) {
    stop("More than two matches discovered, check stargazer output", call. = F)
  }

  star_tbl[tabular_lim[1]:tabular_lim[2]]

}


# merge_stargazer_tbls() -------------------------------------------------------

#' Combine stargazer tables to create tables with multiple panels
#'
#' \code{merge_stargazer_tbls} is a function that combines different stargazer
#' tables into a single table with multiple sub-tables.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param tbls List of table outputs from stargazer
#'
#' @param table_placement Same as \code{table.placement} from \code{stargazer}
#'
#' @param sub_captions Character vector with one element per table. Length of
#'   this vector must be equal to the number of elements in @param tbls .
#'
#' @param cap_width Argument to be passed to width in captionsetup - Latex
#'   parameter.
#'
#' @param tbl_caption Character with text to explain table.
#'
#' @param label Character with label to be used for reference, equal to
#'   \code{label} argument in \code{stargazer}.
#'
#' @param tab_width Argument to be passed to width in adjustbox - Latex
#'   parameter.
#'
#' @param tab_height Argument to be passed to height in adjustbok - Latex
#'   parameter
#'
#' @return Prints the latex code to console
#'

merge_stargazer_tbls <-
  function(tbls,
           table_placement = "!htbp",
           sub_captions = c("Panel A", "Panel B"),
           cap_width = "\\linewidth",
           tbl_caption = "",
           label = "",
           tab_width = "!",
           tab_height = "!") {

    n_tbls <- length(tbls)
    n_sub_captions <- length(sub_captions)

    if (n_tbls != n_sub_captions) {
      stop("Number of tables does not equal number of sub captions", call. = F)
    }

    sub_tbls <-
      purrr::map2(.x = tbls[-n_tbls],
                  .y = sub_captions[-n_sub_captions],
                  ~{

                    c(paste0("   \\begin{minipage}{", cap_width, "}"),
                      paste0("   \\subcaption*{\\footnotesize ", .y, "}"),
                      paste0("   \\vspace{-12pt}"),
                      paste0("   \\end{minipage}"),
                      paste0("\\resizebox{",tab_width,"}{",tab_height,"}{%"),
                      capture_stargazer_tab(.x),
                      "}",
                      "   \\smallskip")

                  }) %>%
      purrr::flatten_chr(.data)

    main_tbl_start <-

      c(paste0("\\begin{table}[", table_placement, "] \\centering "),
        paste0("  \\captionsetup{justification=justified,singlelinecheck=off, font = footnotesize, width = ", cap_width, "}"),
        paste0("   \\caption{", tbl_caption, "}"),
        paste0("   \\label{", label, "}"),
        paste0("   \\smallskip"))


    sub_tbl_fin <-
      c(paste0("   \\begin{minipage}{", cap_width, "}"),
        paste0("   \\subcaption*{\\footnotesize ",
               sub_captions[n_sub_captions], "}"),
        paste0("   \\vspace{-12pt}"),
        paste0("   \\end{minipage}"),
        paste0("\\resizebox{",tab_width,"}{",tab_height,"}{%"),
        capture_stargazer_tab(tbls[[n_tbls]]),
        "}")

    main_tbl_end <- "\\end{table} "


    mult_tbl <-
      c(main_tbl_start, sub_tbls, sub_tbl_fin, main_tbl_end)

    #cat(mult_tbl, sep = "\n")

    mult_tbl

  }


# resize_stargazer() -----------------------------------------------------------

#' Put stargazer table in resize box to fit pages
#'
#' \code{resize_stargazer} is a function around the \code{stargazer}
#' function that allows the user to wrap a stargazer table in a resizebox to
#' easliy fit the table to a page. The tuning is done through the
#' \code{tab_width} and \code{tab_height}. Otherwise use the function as you
#' would use the stargazer function.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' Note: Not my own function, from:
#' https://stackoverflow.com/questions/43245920/how-to-resize-tables-generated-by-stargazer-in-r-markdown
#'
#' @param ... Parameters passed to \code{stargazer}
#'
#' @param tab_width TeX command form for table width
#'
#' @param tab_height TeX command form for table height
#'
#' @param tbl_caption Character, the caption for the table
#'
#' @param cap_width TeX command form for width of table caption
#'
#' @return Latex code for the resized table
#'
#'
#' @importFrom utils capture.output
#' @importFrom purrr prepend
#' @importFrom stargazer stargazer

resize_stargazer <-  function(...,
                              tab_width = "!",
                              tab_height = "!",
                              tbl_caption = "!",
                              cap_width = "\\linewidth"){

  #Extract the code returned from stargazer()
  res <-  utils::capture.output(
    stargazer::stargazer(...)
  )

  #Render the arguments:
  tab_width <-  tab_width
  tab_height <-  tab_height

  #Attach "}" between \end{tabular} and \end{table}
  res <-
    purrr::prepend(res, "}", before = length(res))

  #Input \resizebox before \begin{tabular}
  res <-
    c(res[1:stringr::str_which(res, "^\\\\begin\\{tabular\\}.*") - 1],
      paste0("\\resizebox{",tab_width,"}{",tab_height,"}{%"),
      res[stringr::str_which(res, "^\\\\begin\\{tabular\\}.*"):length(res)]
    )

  # Attach caption info on top of table after \begin{table} and before
  # \resizebox

  res[3] <- paste0("   \\caption{", tbl_caption, "}")

  res <-
    purrr::prepend(res,
                   paste0("\\captionsetup{justification=justified,singlelinecheck=off, font = footnotesize, width = ", cap_width, "}"),
                   before = str_which(res, "^\\\\begin\\{table\\}.*") + 1)

  #Produce the whole strings
  #cat(res, sep = "\n")

  res
}


