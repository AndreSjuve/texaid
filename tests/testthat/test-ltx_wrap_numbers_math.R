library(testthat)

test_that("ltx_wrap_numbers_math wraps numbers correctly", {
  latex_input <- c("Value: 1234", "Temperature: 98.6", "Negative: -567.89")
  expected_output <- c("Value: $1234$", "Temperature: $98.6$", "Negative: $-567.89$")

  result <- unname(ltx_wrap_numbers_math(latex_input, print_tbl = FALSE))
  expect_equal(result, expected_output)
})

test_that("ltx_wrap_numbers_math skips numbers already wrapped in math mode", {
  latex_input <- c("Value: $1234$", "Temperature: 98.6", "Negative: $-567.89$")
  expected_output <- c("Value: $1234$", "Temperature: $98.6$", "Negative: $-567.89$")

  result <- unname(ltx_wrap_numbers_math(latex_input, print_tbl = FALSE))
  expect_equal(result, expected_output)
})

test_that("ltx_wrap_numbers_math handles scientific notation", {
  latex_input <- c("Scientific: 1.23e4", "Another: -5.67e-3")
  expected_output <- c("Scientific: $1.23e4$", "Another: $-5.67e-3$")

  result <- unname(ltx_wrap_numbers_math(latex_input, print_tbl = FALSE))
  expect_equal(result, expected_output)
})

test_that("ltx_wrap_numbers_math handles superscripts", {
  latex_input <- c("Power: 10^{2}", "Exponent: 2^{3}")
  expected_output <- c("Power: $10^{2}$", "Exponent: $2^{3}$")

  result <- unname(ltx_wrap_numbers_math(latex_input, print_tbl = FALSE))
  expect_equal(result, expected_output)
})

test_that("ltx_wrap_numbers_math handles numbers with commas", {
  latex_input <- c("Population: 1,234,567", "Amount: 2,345.67")
  expected_output <- c("Population: $1,234,567$", "Amount: $2,345.67$")

  result <- unname(ltx_wrap_numbers_math(latex_input, print_tbl = FALSE))
  expect_equal(result, expected_output)
})

test_that("ltx_wrap_numbers_math ignores non-numeric content", {
  latex_input <- c("Random text", "No numbers here")
  expected_output <- c("Random text", "No numbers here")

  result <- unname(ltx_wrap_numbers_math(latex_input, print_tbl = FALSE))
  expect_equal(result, expected_output)
})


test_that("ltx_wrap_numbers_math wraps numbers correctly in math mode", {

  # Test with basic LaTeX input with integers
  latex_input <- c("Value: 1234.", "Temperature: 98.6F", "Negative: -567.89!")
  result <- unname(ltx_wrap_numbers_math(latex_input))

  # Check if the numbers are wrapped in math mode
  expect_equal(result[1], "Value: $1234$.")
  expect_equal(result[2], "Temperature: $98.6F$")
  expect_equal(result[3], "Negative: $-567.89$!")
})

test_that("ltx_wrap_numbers_math handles commas in numbers", {

  # Test with numbers containing commas
  latex_input <- c("Revenue: 1,234,567.", "Cost: -2,345,678.90")
  result <- unname(ltx_wrap_numbers_math(latex_input))

  # Check if the numbers are wrapped in math mode with commas preserved
  expect_equal(result[1], "Revenue: $1,234,567$.")
  expect_equal(result[2], "Cost: $-2,345,678.90$")
})

test_that("ltx_wrap_numbers_math handles scientific notation", {

  # Test with numbers in scientific notation
  latex_input <- c("Large value: 1.23e10.", "Small value: -4.56e-9.")
  result <- unname(ltx_wrap_numbers_math(latex_input))

  # Check if the numbers in scientific notation are wrapped in math mode
  expect_equal(result[1], "Large value: $1.23e10$.")
  expect_equal(result[2], "Small value: $-4.56e-9$.")
})

test_that("ltx_wrap_numbers_math doesn't wrap already wrapped numbers", {

  # Test with already wrapped numbers
  latex_input <- c("Value: $1234.$", "Temperature: $98.6F$", "Negative: $-567.89!$")
  result <- unname(ltx_wrap_numbers_math(latex_input))

  # Ensure the function doesn't double-wrap already wrapped numbers
  expect_equal(result[1], "Value: $1234.$")
  expect_equal(result[2], "Temperature: $98.6F$")
  expect_equal(result[3], "Negative: $-567.89!$")
})

test_that("ltx_wrap_numbers_math handles negative and positive numbers correctly", {

  # Test with both positive and negative numbers
  latex_input <- c("Positive: 5678.", "Negative: -9876.", "Mixed: -1.23 and 456.78.")
  result <- unname(ltx_wrap_numbers_math(latex_input))

  # Check if both positive and negative numbers are wrapped in math mode
  expect_equal(result[1], "Positive: $5678$.")
  expect_equal(result[2], "Negative: $-9876$.")
  expect_equal(result[3], "Mixed: $-1.23$ and $456.78$.")
})

test_that("ltx_wrap_numbers_math handles no numbers correctly", {

  # Test with no numbers
  latex_input <- c("No numbers here.", "Still no numbers.")
  result <- unname(ltx_wrap_numbers_math(latex_input))

  # Ensure the text remains unchanged when there are no numbers
  expect_equal(result[1], "No numbers here.")
  expect_equal(result[2], "Still no numbers.")
})

