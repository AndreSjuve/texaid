library(testthat)
library(ggplot2)

# Mocking the functions save_tikz and ltx_build_fig
save_tikz <- function(fig, path, sanitize, width, height) {
  # Simulate saving a figure successfully
  return(TRUE)
}

ltx_build_fig <- function(fig_in, fig_out, title, caption, label, copy_fig) {
  # Simulate building the figure successfully
  return(TRUE)
}

# Test suite for render_ggplot_to_latex
test_that("render_ggplot_to_latex handles NULL inputs", {
  expect_error(render_ggplot_to_latex(NULL, "path/to/figure.tex"),
               "fig is NULL, provide a figure to save.")

  expect_error(render_ggplot_to_latex(ggplot(mtcars, aes(wt, mpg)), NULL),
               "fig_path is NULL, don't know where to save the figure file.")
})

test_that("render_ggplot_to_latex checks for ggplot object", {
  expect_error(render_ggplot_to_latex("not_a_ggplot", "path/to/figure.tex"),
               "The provided figure is not a valid ggplot object.")
})

