library(testthat)

test_that("round_latex_numbers correctly rounds numbers in LaTeX tables", {
  latex_table <- c(
    "\\begin{tabular}{|c|c|c|}",
    "\\hline",
    "Value A & Value B & Value C \\\\",
    "\\hline",
    "1.234 & 10.5678 & 12345.6789 \\\\",
    "2.345 & 9.8765 & 6789.0123 \\\\",
    "\\hline",
    "\\end{tabular}"
  )

  # Test rounding behavior
  rounded_table <- ltx_round_numbers(latex_table,
                                     threshold = 10,
                                     big_dec = 0,
                                     small_dec = 2)

  expect_equal(
    rounded_table,
    c(
      "\\begin{tabular}{|c|c|c|}",
      "\\hline",
      "Value A & Value B & Value C \\\\",
      "\\hline",
      "1.23 & 11 & 12,346 \\\\",
      "2.35 & 9.88 & 6,789 \\\\",
      "\\hline",
      "\\end{tabular}"
    )
  )

  # Test with a different threshold and decimal places
  rounded_table <- ltx_round_numbers(latex_table, threshold = 5, big_dec = 1, small_dec = 3)

  expect_equal(
    rounded_table,
    c(
      "\\begin{tabular}{|c|c|c|}",
      "\\hline",
      "Value A & Value B & Value C \\\\",
      "\\hline",
      "1.234 & 10.6 & 12,345.7 \\\\",
      "2.345 & 9.9 & 6,789 \\\\",
      "\\hline",
      "\\end{tabular}"
    )
  )

  # Test with a matrix input
  matrix_table <- matrix(c(
    "Value A", "Value B", "Value C",
    "1.234", "10.5678", "12345.6789",
    "2.345", "9.8765", "6789.0123"
  ), nrow = 3, byrow = TRUE)

  rounded_table <- ltx_round_numbers(matrix_table, threshold = 10)

  expect_equal(
    rounded_table,
    c(
      "Value A & Value B & Value C",
      "1.23 & 11 & 12,346",
      "2.35 & 9.88 & 6,789"
    )
  )
})

test_that("round_latex_numbers handles invalid input gracefully", {
  expect_error(ltx_round_numbers(NULL, threshold = 10),
               "latex_table must be a character vector, data frame, or matrix.")

  expect_error(ltx_round_numbers(123, threshold = 10),
               "latex_table must be a character vector, data frame, or matrix.")

  expect_error(ltx_round_numbers("invalid", threshold = "not numeric"),
               "threshold must be a single numeric value.")
})
