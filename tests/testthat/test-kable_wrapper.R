library(testthat)
test_that("kable_wrapper handles NULL input", {
  expect_error(kable_wrapper(tbl = NULL, title = "Sample Table", lbl = "tab:sample"),
               "No table given")
})

test_that("kable_wrapper works for data frames", {
  df <- data.frame(A = 1:3, B = c("X", "Y", "Z"))
  result <- kable_wrapper(tbl = df, title = "Sample Table", lbl = "sample", print_tbl = FALSE)
  expect_true(grepl("\\\\caption\\{\\\\label\\{tab:sample\\}Sample Table\\}", result))  # Check for caption
  expect_true(grepl("\\\\caption\\{\\\\label\\{tab:sample\\}Sample Table\\}", result))      # Check for label
})

test_that("kable_wrapper handles tibble input", {
  tb <- tibble::tibble(A = 1:3, B = c("X", "Y", "Z"))
  result <- kable_wrapper(tbl = tb, title = "Tibble Table", lbl = "tibble_sample", print_tbl = FALSE)
  expect_true(grepl("\\\\caption\\{\\\\label\\{tab:tibble_sample\\}Tibble Table\\}", result))  # Check for caption
  expect_true(grepl("\\\\caption\\{\\\\label\\{tab:tibble_sample\\}Tibble Table\\}", result)) # Check for label
})

test_that("kable_wrapper uses default column specifications when NULL", {
  df <- data.frame(A = 1:3, B = c("X", "Y", "Z"))
  result <- kable_wrapper(tbl = df, title = "Default Column Spec", lbl = "tab:default", print_tbl = FALSE)
  expect_true(grepl("l", result))  # Check for left alignment for the first column
  expect_true(grepl("c", result))  # Check for center alignment for subsequent columns
})

test_that("kable_wrapper raises an error for mismatched column specifications", {
  df <- data.frame(A = 1:3, B = c("X", "Y", "Z"))
  expect_error(kable_wrapper(tbl = df, title = "Mismatched Columns", lbl = "tab:mismatch",
                             col_spec = c("l", "c", "r", "r")),
               "Length of column specification does not match the number of columns in the tbl data frame")
})

test_that("kable_wrapper warns for invalid col_type", {
  df <- data.frame(A = 1:3, B = c("X", "Y", "Z"))
  expect_message(kable_wrapper(tbl = df, title = "Invalid Column Type", lbl = "tab:invalid", col_type = "invalid"),
                 "col_type not l, c or r, defaulting to c")
})

# Additional tests can be added to check the functionality under different conditions.
