library(testthat)


test_that("ltx_resize works for table environments", {
table_code <-  c(
  "\\begin{table}",
  "\\centering",
  "\\begin{tabular}{cc}",
  "A & B \\\\",
  "1 & 2 \\\\",
  "\\end{tabular}",
  "\\end{table}"
)
  result <- ltx_resize(table_code)

  expect_true(grepl("\\\\resizebox", result[3]))
  expect_true(grepl("\\}\\}$", result[7]))
})


test_that("ltx_resize returns NULL if no valid environment is found", {
  no_env_code <- c("This is a test", "No environment here")
  result <- ltx_resize(no_env_code)

  expect_null(result)  # Should return NULL if no environment is found
})


