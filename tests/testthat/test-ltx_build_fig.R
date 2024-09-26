library(testthat)

test_that("ltx_build_fig works correctly", {

  # Create a temporary directory for testing
  temp_dir <- tempdir()
  fig_in_path <- file.path(temp_dir, "fig_in_test.tex")
  fig_out_path <- file.path(temp_dir, "fig_out_test.tex")

  # Write a sample TikZ figure to fig_in
  writeLines(c(
    "\\begin{tikzpicture}",
    "\\draw (0,0) -- (1,1);",
    "\\end{tikzpicture}"
  ), fig_in_path)

  # Test with required fig_in parameter
  expect_error(ltx_build_fig(fig_in = NULL), "fig_in must be supplied. Argument is NULL")
  expect_error(ltx_build_fig(fig_in = "non_existing_file.tex"), "fig_in not found. File does not exist, check directory.")

  # Test with default values and copy_fig
  result <- ltx_build_fig(fig_in = fig_in_path, fig_out = fig_out_path)

  # Check if the output file is created
  expect_true(file.exists(fig_out_path))

  # Check the content of the output file
  output_content <- readLines(fig_out_path)
  expect_true(grepl("\\\\begin\\{figure\\}\\[htbp!\\]", output_content[1]))
  expect_true(grepl("\\\\caption\\{\\\\header\\{Example figure\\} \\\\", output_content[3]))
  expect_true(grepl("\\\\label\\{fig:example\\}", output_content[5]))

  # Test with specified title, caption, and label
  custom_title <- "Custom Title"
  custom_caption <- "This is a custom caption."
  custom_label <- "custom_label"

  custom_result <- ltx_build_fig(fig_in = fig_in_path,
                                 title = custom_title,
                                 caption = custom_caption,
                                 label = custom_label)

  # Check the updated output content
  custom_output_content <- readLines(fig_out_path)
  expect_true(grepl("\\\\caption\\{\\\\header\\{Custom Title\\} \\\\", custom_output_content[3]))
  expect_true(grepl("\\\\label\\{fig:custom_label\\}", custom_output_content[5]))

  # Clean up test files
  file.remove(fig_out_path)
})
