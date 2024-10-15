test_that("ltx_wrap_table_math works correctly for tabular environments", {
  # Example LaTeX input with numbers
  latex_input <- c(
    "\\begin{tabular}{|c|c|}",
    "Value: 1234",
    "Temperature: 98.6",
    "Negative: -567.89",
    "\\end{tabular}"
  )

  # Expected output where numbers are wrapped in math mode
  expected_output <- c(
    "\\begin{tabular}{|c|c|}",
    "Value: $1234$",
    "Temperature: $98.6$",
    "Negative: $-567.89$",
    "\\end{tabular}"
  )

  # Test the function
  result <- ltx_wrap_table_math(latex_input, ruler_based = FALSE, print_tbl = FALSE)
  expect_equal(result, expected_output)
})


test_that("ltx_wrap_table_math handles multiple tabular environments", {
  # LaTeX input with two tabular environments
  latex_input <- c(
    "Some text outside tabular",
    "\\begin{tabular}{|c|c|}",
    "Value: 1234",
    "\\end{tabular}",
    "Other text",
    "\\begin{tabular}{|c|c|}",
    "Temperature: 98.6",
    "\\end{tabular}"
  )

  # Expected output with both tabular environments processed
  expected_output <- c(
    "Some text outside tabular",
    "\\begin{tabular}{|c|c|}",
    "Value: $1234$",
    "\\end{tabular}",
    "Other text",
    "\\begin{tabular}{|c|c|}",
    "Temperature: $98.6$",
    "\\end{tabular}"
  )

  # Test the function
  result <- ltx_wrap_table_math(latex_input, ruler_based = FALSE, print_tbl = FALSE)
  expect_equal(result, expected_output)
})

test_that("ltx_wrap_table_math skips already wrapped numbers", {
  # LaTeX input with numbers already in math mode
  latex_input <- c(
    "\\begin{tabular}{|c|c|}",
    "Value: $1234$",
    "Temperature: 98.6",
    "Negative: $-567.89$",
    "\\end{tabular}"
  )

  # Expected output only wraps the number not already in math mode
  expected_output <- c(
    "\\begin{tabular}{|c|c|}",
    "Value: $1234$",
    "Temperature: $98.6$",
    "Negative: $-567.89$",
    "\\end{tabular}"
  )

  # Test the function
  result <- ltx_wrap_table_math(latex_input, ruler_based = FALSE)
  expect_equal(result, expected_output)
})
