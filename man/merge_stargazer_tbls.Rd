% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stargazer.R
\name{merge_stargazer_tbls}
\alias{merge_stargazer_tbls}
\title{Combine stargazer tables to create tables with multiple panels}
\usage{
merge_stargazer_tbls(
  tbls,
  table_placement = "!htbp",
  sub_captions = c("Panel A", "Panel B"),
  cap_width = "\\\\linewidth",
  tbl_caption = "",
  label = "",
  tab_width = "!",
  tab_height = "!"
)
}
\arguments{
\item{tbls}{List of table outputs from stargazer}

\item{table_placement}{Same as \code{table.placement} from \code{stargazer}}

\item{sub_captions}{Character vector with one element per table. Length of
this vector must be equal to the number of elements in @param tbls .}

\item{cap_width}{Argument to be passed to width in captionsetup - Latex
parameter.}

\item{tbl_caption}{Character with text to explain table.}

\item{label}{Character with label to be used for reference, equal to
\code{label} argument in \code{stargazer}.}

\item{tab_width}{Argument to be passed to width in adjustbox - Latex
parameter.}

\item{tab_height}{Argument to be passed to height in adjustbok - Latex
parameter}
}
\value{
Prints the latex code to console
}
\description{
\code{merge_stargazer_tbls} is a function that combines different stargazer
tables into a single table with multiple sub-tables.
}
\details{
Used in scripts: {}
Robust function: FALSE
}
