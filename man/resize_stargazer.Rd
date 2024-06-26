% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stargazer.R
\name{resize_stargazer}
\alias{resize_stargazer}
\title{Put stargazer table in resize box to fit pages}
\usage{
resize_stargazer(
  ...,
  tab_width = "!",
  tab_height = "!",
  tbl_caption = "!",
  cap_width = "\\\\linewidth"
)
}
\arguments{
\item{...}{Parameters passed to \code{stargazer}}

\item{tab_width}{TeX command form for table width}

\item{tab_height}{TeX command form for table height}

\item{tbl_caption}{Character, the caption for the table}

\item{cap_width}{TeX command form for width of table caption}
}
\value{
Latex code for the resized table
}
\description{
\code{resize_stargazer} is a function around the \code{stargazer}
function that allows the user to wrap a stargazer table in a resizebox to
easliy fit the table to a page. The tuning is done through the
\code{tab_width} and \code{tab_height}. Otherwise use the function as you
would use the stargazer function.
}
\details{
Used in scripts: {}
Robust function: FALSE

Note: Not my own function, from:
https://stackoverflow.com/questions/43245920/how-to-resize-tables-generated-by-stargazer-in-r-markdown
}
