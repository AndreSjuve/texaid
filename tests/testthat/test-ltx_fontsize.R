library(testthat)
test_that("ltx_fontsize handles valid LaTeX input", {
  # Sample LaTeX table code
  table_code <- c("\\begin{table}", "\\centering", "\\begin{tabular}{cc}", "A & B \\\\", "1 & 2 \\\\", "\\end{tabular}", "\\end{table}")

  # Test with valid font_size and skip
  modified_code <- ltx_fontsize(table_code, font_size = 10, skip = 12, print_tbl = FALSE, verbose = FALSE)
  expect_true(any(stringr::str_detect(modified_code, "\\\\fontsize\\{10\\}\\{12\\}\\\\selectfont")))

  # Test when skip is NULL (default skip = 1.2 * font_size)
  modified_code <- ltx_fontsize(table_code, font_size = 10, skip = NULL, print_tbl = FALSE, verbose = FALSE)
  expect_true(any(stringr::str_detect(modified_code, "\\\\fontsize\\{10\\}\\{12\\}\\\\selectfont")))  # skip should be 1.2 * 10 = 12
})

test_that("ltx_fontsize handles missing \\begin{table} tag", {
  # LaTeX code without the \\begin{table} tag
  table_code <- c("\\centering", "\\begin{tabular}{cc}", "A & B \\\\", "1 & 2 \\\\", "\\end{tabular}")

  # Test should fail if \\begin{table} is missing
  expect_error(ltx_fontsize(table_code, font_size = 10, skip = 12, print_tbl = FALSE, verbose = FALSE),
               "No 'begin table' tag found in the LaTeX input.")
})

test_that("ltx_fontsize handles invalid font_size and skip", {
  # Sample LaTeX table code
  table_code <- c("\\begin{table}", "\\centering", "\\begin{tabular}{cc}", "A & B \\\\", "1 & 2 \\\\", "\\end{tabular}", "\\end{table}")

  # Test with invalid font size (e.g., negative)
  expect_error(ltx_fontsize(table_code, font_size = -5, skip = 12, print_tbl = FALSE, verbose = FALSE),
               "Font size must be a positive number.")

  # Test with invalid skip (e.g., zero)
  expect_error(ltx_fontsize(table_code, font_size = 10, skip = 0, print_tbl = FALSE, verbose = FALSE),
               "Skip must be a positive number.")
})

test_that("ltx_fontsize appends \\fontsize correctly", {
  # Sample LaTeX table code
  table_code <- c("\\begin{table}", "\\centering", "\\begin{tabular}{cc}", "A & B \\\\", "1 & 2 \\\\", "\\end{tabular}", "\\end{table}")

  # Modify font size to 10 and check for the correct placement of the \\fontsize command
  modified_code <- ltx_fontsize(table_code, font_size = 10, skip = 12, print_tbl = FALSE, verbose = FALSE)

  tbl_start <- which(stringr::str_detect(modified_code, "\\\\begin\\{table\\}"))
  fontsize_line <- which(stringr::str_detect(modified_code, "\\\\fontsize"))

  expect_true(fontsize_line == tbl_start + 1)  # The fontsize should appear right after \\begin{table}
})
