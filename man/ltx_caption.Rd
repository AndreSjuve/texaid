% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ltx_caption.R
\name{ltx_caption}
\alias{ltx_caption}
\title{Process LaTeX Table Header with Caption, Label, and Note}
\usage{
ltx_caption(x, tbl_note = "", print_tbl = FALSE)
}
\arguments{
\item{x}{A character vector representing the LaTeX table code
that needs to be processed.}

\item{tbl_note}{A character string for an optional table note that will be
added to the table header.}

\item{print_tbl}{Logical. If \code{TRUE}, the processed LaTeX table will be
printed to the console. Defaults to \code{FALSE}.}
}
\value{
The function returns the processed LaTeX table code as an invisible
character vector. If \code{print_tbl} is set to \code{TRUE}, it also prints the
modified table code to the console and copies it to the clipboard for easy
pasting into a latex editor.
}
\description{
The \code{ltx_caption} function processes LaTeX table code by modifying the header,
including the caption, label, and table notes. It removes unnecessary lines,
captures the table title, and optionally appends a label and table note,
while also printing the table if specified.
}
\details{
This function refines LaTeX table code by removing unwanted \verb{\\\\usepackage}
commands and blank lines, capturing and modifying the \verb{\\\\caption} and
\verb{\\\\label}, and optionally adding a custom table note. The function
identifies the LaTeX table title and reformats it into a custom format.
If a table label is present in the title, it is extracted and repositioned
before the table content. If a table note is provided, it is
appended after the caption.
}
\examples{
\dontrun{
# Example LaTeX table with caption, label, and table note
tbl_code <- c("\\\\usepackage{booktabs}",
              "\\\\caption{Sample Table}",
              "\\\\label{tab:example}",
              "\\\\begin{tabular}{|c|c|}",
              "\\\\hline",
              "A & B \\\\\\\\",
              "\\\\hline",
              "1 & 2 \\\\\\\\",
              "\\\\hline",
              "\\\\end{tabular}")

# Process the table header with a table note and print the table
ltx_caption(tbl_code, tbl_note = "Table Note", print_tbl = TRUE)
}
}
\seealso{
\code{\link[=ltx_print_tbl]{ltx_print_tbl()}}, \code{\link[=ltx_capture_output]{ltx_capture_output()}}
}
