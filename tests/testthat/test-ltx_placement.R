library(testthat)
# Sample LaTeX code for testing
latex_code <-  c(
     "\\begin{table}",
     "\\centering",
     "\\begin{tabular}{cc}",
     "A & B \\\\",
     "1 & 2 \\\\",
     "\\end{tabular}",
     "\\end{table}",
     "\\begin{table}",
     "\\begin{tabular}{cc}",
     "A & B \\\\",
     "1 & 2 \\\\",
     "\\end{tabular}",
     "\\end{table}"
   )

# Test that the function replaces table placements correctly
test_that("ltx_placement replaces table placement specifier", {
  modified_code <- ltx_placement(latex_code, tbl_placement = "!htbp")

  expect_true(any(grepl("\\\\begin\\{table\\}\\[!htbp\\]", modified_code)))
  expect_false(any(grepl("^\\\\begin\\{table\\}$", modified_code)))  # Original placement should be replaced
})

# Test that the function defaults to '!htbp' if tbl_placement is NULL
test_that("ltx_placement defaults to '!htbp' when tbl_placement is NULL", {
  modified_code <- ltx_placement(latex_code, tbl_placement = NULL)

  expect_true(any(grepl("\\\\begin\\{table\\}\\[!htbp\\]", modified_code)))
})


# Test that the function handles specific index replacements
test_that("ltx_placement replaces specific table placement", {
  modified_code <- ltx_placement(latex_code, tbl_placement = "H", replace_idx = 1)

  # Check that the first table has 'H' as its placement
  expect_true(grepl("\\\\begin\\{table\\}\\[H\\]", modified_code[1]))

  # Check that the second table remains unchanged
  expect_true(grepl("\\\\begin\\{table\\}", modified_code[8]))
})

# Test that the function returns the modified LaTeX code
test_that("ltx_placement returns modified LaTeX code", {
  result <- ltx_placement(latex_code, tbl_placement = "!htbp")
  expect_type(result, "character")
  expect_equal(length(result), length(latex_code))  # Length should remain the same
})



