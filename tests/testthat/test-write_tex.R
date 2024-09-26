library(testthat)

# Test for writing a TeX file
test_that("write_tex creates a .tex file correctly", {
  temp_file <- tempfile(fileext = ".tex")
  content <- c("\\documentclass{article}", "\\begin{document}", "Hello, world!", "\\end{document}")

  # Write the file
  write_tex(content, temp_file)

  # Check if the file exists
  expect_true(file.exists(temp_file))

  # Check the content of the file
  written_content <- readLines(temp_file)
  expect_equal(written_content, content)

  # Clean up
  unlink(temp_file)
})

# Test for directory creation
test_that("write_tex creates the directory if it does not exist", {
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  temp_file <- file.path(temp_dir, "output.tex")

  # Clean up in case the file already exists
  if (file.exists(temp_file)) unlink(temp_file)

  # Write the file
  write_tex(c("Some content"), temp_file)

  # Check if the file exists
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
  unlink(temp_dir, recursive = TRUE)
})

# Test for overwriting existing file
test_that("write_tex overwrites existing file", {
  temp_file <- tempfile(fileext = ".tex")
  initial_content <- c("Initial content")
  new_content <- c("New content")

  # Write the initial content
  write_tex(initial_content, temp_file)

  # Write the new content, expecting it to overwrite the existing file
  write_tex(new_content, temp_file)

  # Check the content of the file
  written_content <- readLines(temp_file)
  expect_equal(written_content, new_content)

  # Clean up
  unlink(temp_file)
})

# Test for warning when file extension is incorrect
test_that("write_tex warns and changes extension to .tex", {
  temp_file <- tempfile(fileext = ".txt")

  expect_warning({
    write_tex(c("Some content"), temp_file)
  }, "File extension is not .tex; changing extension to .tex")

  # Check if the file exists with the correct extension
  expect_true(file.exists(fs::path_ext_set(temp_file, ext = "tex")))

  # Clean up
  unlink(fs::path_ext_set(temp_file, ext = "tex"))
})
