library(testthat)

# Test for deprecation warning
#test_that("ltx_multipanel gives a deprecation warning", {
#  expect_warning({
#    ltx_multipanel(list(c("\\begin{tabular}", "\\bottomrule", "\\end{tabular}"),
#                        c("\\begin{tabular}", "\\resizebox{\\linewidth}{!}{", "\\end{tabular}")))
#  }, "The `ltx_multipanel()` function is deprecated and will be removed in future versions. Please do not use it.")
#})

# Test for the output format
test_that("ltx_multipanel combines panels correctly", {
  panel1 <- c("\\begin{tabular}", "\\bottomrule", "\\end{tabular}")
  panel2 <- c("\\begin{tabular}", "\\resizebox{\\linewidth}{!}{", "\\end{tabular}")

  result <- ltx_multipanel(list(panel1, panel2))

  expect_true(grepl("\\\\begin\\{tabular\\}", result[1])) # Check for begin tabular
  #expect_false(grepl("\\\\bottomrule", result)) # Ensure bottomrule was removed
  expect_true(any(grepl("\\\\resizebox", result))) # Ensure resizebox is present
  expect_true(grepl("\\\\end\\{tabular\\}", result[length(result)])) # Check for end tabular
})

# Test for printing and clipboard functionality
#test_that("ltx_multipanel prints and copies to clipboard", {
#  panel1 <- c("\\begin{tabular}", "\\bottomrule", "\\end{tabular}")
#  panel2 <- c("\\begin{tabular}", "\\resizebox{\\linewidth}{!}{", "\\end{tabular}")
#
#  expect_output(
#    ltx_multipanel(list(panel1, panel2), print_tbl = TRUE),
#    regexp = ".*"
#  ) # Ensure output is printed
#})
