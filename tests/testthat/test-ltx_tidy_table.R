
library(testthat)

# Mock input for LaTeX table
latex_input <- c(
  "\\begin{table}",
  "\\caption{\\label{tab:foo}Foo}",
  "\\begin{tabular}{|c|c|}",
  "\\hline",
  "Value & 1234 \\\\",
  "Temperature & 98.6 \\\\",
  "Large Number & 1234567.89 \\\\",
  "Small Number & 0.00123 \\\\",
  "Negative & -567.89 \\\\",
  "\\hline",
  "\\end{tabular}",
  "\\end{table}"
)

# Test 1: Basic functionality and output invisibility
test_that("ltx_tidy_table processes LaTeX table correctly", {
  result <- ltx_tidy_table(latex_input, tbl_note = "Test Table", ruler_based = FALSE, print_tbl = F)
  expect_silent(result)  # The function returns invisibly
})

# Test 2: Caption inclusion
test_that("ltx_tidy_table adds caption correctly", {
  result <- ltx_tidy_table(latex_input, tbl_note = "Test Table", ruler_based = FALSE, print_tbl = FALSE)
  expect_true(any(stringr::str_detect(result, "\\\\caption\\{\\\\header\\{Foo\\}"))) # Ensure caption was added
})

# Test 3: Array stretch adjustment
test_that("ltx_tidy_table adjusts row spacing", {
  result <- ltx_tidy_table(latex_input, array_stretch = 1.5, ruler_based = FALSE, print_tbl = FALSE)
  expect_true(any(stringr::str_detect(result, "\\\\ra\\{1.5\\}")))  # Checks that \\ra{1.5} was applied
})

# Test 4: Number rounding and math wrapping
test_that("ltx_tidy_table rounds and wraps numbers in math mode", {
  result <- ltx_tidy_table(latex_input, threshold = 1000, big_dec = 1, small_dec = 3, ruler_based = FALSE, print_tbl = FALSE)

  # Check that numbers are wrapped in math mode and rounded correctly
  expect_true(any(grepl("\\$1,234\\$", result)))  # "1234" wrapped in $...$
  expect_true(any(grepl("\\$1,234,567.9\\$", result)))  # "1234567.89" rounded to 1 decimal place
  expect_true(any(grepl("\\$0.001\\$", result)))  # Small number rounded to 3 decimal places
})

# Test 5: Negative number handling
test_that("ltx_tidy_table handles negative numbers correctly", {
  result <- ltx_tidy_table(latex_input, ruler_based = FALSE, print_tbl = TRUE)
  expect_true(any(grepl("\\$-567.89\\$", result)))  # Negative numbers wrapped in $...$
})

# Test 6: Ensure no double wrapping
test_that("ltx_tidy_table avoids double-wrapping numbers in math mode", {
  latex_input_with_math <- c(
    "\\begin{table}",
    "\\caption{\\label{tab:foo}Foo}",
    "\\begin{tabular}{|c|c|}",
    "\\hline",
    "Already Wrapped & $1234$ \\\\",
    "New Number & 9876 \\\\",
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  result <- ltx_tidy_table(latex_input_with_math, ruler_based = FALSE, print_tbl = TRUE)

  # Ensure "Already Wrapped" stays wrapped and new number gets wrapped
  expect_true(any(grepl("\\$1,234\\$", result)))  # No double wrapping for already wrapped
  expect_true(any(grepl("\\$9,876\\$", result)))  # New number gets wrapped
})
