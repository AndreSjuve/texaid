library(testthat)
# Test cases for ltx_capture_output function
test_that("ltx_capture_output handles single input correctly", {
  single_input <- "This is a test"
  result <- ltx_capture_output(single_input)
  expect_equal(result, capture.output(single_input))  # Expect captured output to match R's capture.output
})

test_that("ltx_capture_output handles multiple input correctly", {
  multiple_input <- c("This is line 1", "This is line 2")
  result <- ltx_capture_output(multiple_input)
  expected_result <- capture.output(cat(multiple_input, sep = "\n"))
  expect_equal(result, expected_result)  # Expect captured output for multiple lines to match
})

test_that("ltx_capture_output returns invisible output", {
  input <- "Invisible test"
  result <- ltx_capture_output(input)
  expect_true(invisible(result) == result)  # Ensure the result is invisible
})

test_that("ltx_capture_output works with numeric input", {
  numeric_input <- 12345
  result <- ltx_capture_output(numeric_input)
  expect_equal(result, capture.output(numeric_input))  # Numeric input should be captured correctly
})

test_that("ltx_capture_output works with mixed type input", {
  mixed_input <- c("Text", 123, TRUE)
  result <- ltx_capture_output(mixed_input)
  expected_result <- capture.output(cat(mixed_input, sep = "\n"))
  expect_equal(result, expected_result)  # Mixed input types should be captured correctly
})
