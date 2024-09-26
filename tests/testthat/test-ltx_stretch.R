library(testthat)

test_that("ltx_stretch works as expected with valid input", {
  # Sample LaTeX table
  latex_table <- c("\\begin{table}",
                   "\\begin{tabular}{ccc}",
                   "\\hline",
                   "1 & 2 & 3 \\\\",
                   "\\hline",
                   "\\end{tabular}",
                   "\\end{table}")

  # Apply stretch
  result <- ltx_stretch(latex_table, array_stretch = 1.5, verbose = FALSE)

  # Check if stretch command is added after \begin{tabular}
  expect_true("\\ra{1.5}" %in% result)
})

test_that("ltx_stretch handles missing tabular environment", {
  # Table without tabular environment
  latex_table <- c("\\begin{tabular}{ccc}",
                   "\\hline",
                   "1 & 2 & 3 \\\\",
                   "\\hline",
                   "\\end{tabular}")

  # Expect an error when no tabular is present
  expect_error(ltx_stretch(latex_table, array_stretch = 1.2),
               "No 'begin table' tag found in the LaTeX input.")
})

test_that("ltx_stretch handles invalid array_stretch values", {
  # Sample LaTeX table
  latex_table <- c("\\begin{table}",
                   "\\begin{tabular}{ccc}",
                   "\\hline",
                   "1 & 2 & 3 \\\\",
                   "\\hline",
                   "\\end{tabular}",
                   "\\end{table}")

  # Expect an error for non-positive stretch values
  expect_error(ltx_stretch(latex_table, array_stretch = -1),
               "Array stretch must be a positive numeric value.")

  expect_error(ltx_stretch(latex_table, array_stretch = "a"),
               "Array stretch must be a positive numeric value.")
})

test_that("ltx_stretch provides verbose output", {
  # Sample LaTeX table
  latex_table <- c("\\begin{table}",
                   "\\begin{tabular}{ccc}",
                   "\\hline",
                   "1 & 2 & 3 \\\\",
                   "\\hline",
                   "\\end{tabular}",
                   "\\end{table}")

  # Capture the verbose message
  expect_message(ltx_stretch(latex_table, array_stretch = 1.2, verbose = TRUE),
                 "Array stretch set to 1.2.")
})

