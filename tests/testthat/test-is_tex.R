library(testthat)
# Test cases for is_tex function
test_that("is_tex returns TRUE for TeX files", {
  expect_true(is_tex("document.tex"))  # Valid .tex file
  expect_true(is_tex("/path/to/file/document.tex"))  # Full path to a .tex file
})

test_that("is_tex returns FALSE for non-TeX files", {
  expect_false(is_tex("image.png"))  # File with .png extension
  expect_false(is_tex("/path/to/file/image.jpeg"))  # Full path to a non-.tex file
})

test_that("is_tex returns FALSE for files with no extension", {
  expect_false(is_tex("file"))  # No file extension
})

test_that("is_tex throws an error for invalid input", {
  expect_error(is_tex(123), "Please specify a correct path.")  # Non-character input
  expect_error(is_tex(TRUE), "Please specify a correct path.")  # Boolean input
})
