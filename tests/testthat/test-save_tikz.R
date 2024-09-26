library(testthat)
library(ggplot2)

# Test case 1: Saving a valid ggplot object
test_that("save_tikz saves a valid ggplot object", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + theme_minimal()

  temp_file <- tempfile(fileext = ".tex")

  save_tikz(fig = p, path = temp_file)

  expect_true(file.exists(temp_file))  # Check if the file is created

  # Clean up
  unlink(temp_file)
})

# Test case 2: Error when no figure is provided
test_that("save_tikz errors when no figure is provided", {
  expect_error(save_tikz(path = "test.tex"), "fig is NULL")
})

# Test case 3: Error when the provided object is not a ggplot
test_that("save_tikz errors when provided object is not a ggplot", {
  not_a_ggplot <- data.frame(x = 1:10, y = rnorm(10))
  expect_error(save_tikz(fig = not_a_ggplot, path = "test.tex"), "The provided figure is not a valid ggplot object")
})

# Test case 4: Error when no path is provided
test_that("save_tikz errors when no path is provided", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + theme_minimal()
  expect_error(save_tikz(fig = p), "path is NULL")
})

# Test case 5: Adds .tex extension when not provided
test_that("save_tikz adds .tex extension when not provided", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + theme_minimal()

  temp_file <- tempfile(fileext = "")

  expect_message(save_tikz(fig = p, path = temp_file), "No file extension given, adding .tex")

  expect_true(file.exists(paste0(temp_file, ".tex")))  # Check if the file is created with .tex

  # Clean up
  unlink(paste0(temp_file, ".tex"))
})

# Test case 6: Error if the directory does not exist
test_that("save_tikz errors if the directory does not exist", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + theme_minimal()
  invalid_path <- file.path("non_existing_directory", "figure.tex")

  expect_error(save_tikz(fig = p, path = invalid_path), "Directory does not exist")
})
