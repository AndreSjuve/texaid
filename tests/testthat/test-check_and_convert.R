library(testthat)
test_that("check_and_convert works for data frames", {
  df <- data.frame(a = 1:3, b = 4:6)
  result <- check_and_convert(df)
  expect_true(is.data.frame(result))
  expect_equal(result$a, df$a)
  expect_equal(result$b, df$b)
})

test_that("check_and_convert works for tibbles", {
  tb <- tibble::tibble(a = 1:3, b = 4:6)
  result <- check_and_convert(tb)
  expect_true(tibble::is_tibble(result))
  expect_equal(result$a, tb$a)
  expect_equal(result$b, tb$b)
})

test_that("check_and_convert converts numeric vectors to tibbles", {
  vec <- c(1, 2, 3)
  result <- check_and_convert(vec)
  expect_true(tibble::is_tibble(result))
  expect_equal(result[[1]], vec)
})

test_that("check_and_convert raises an error for incompatible structures", {
  invalid_data <- as.formula(x ~ y)
  expect_error(check_and_convert(invalid_data),
               "Input cannot be converted to a tibble. Please provide a compatible structure.")
})

#test_that("check_and_convert prints messages when verbose is TRUE", {
#  # Test with a mock of the cli_alert_info function
#  mock_cli_alert_info <- function(...) {
#    message("Mock alert: ", ...)
#  }
#
#  # Replace cli_alert_info with mock
#  with_mocked_(
#    cli::cli_alert_info = mock_cli_alert_info
#    {
#      df <- data.frame(a = 1:3, b = 4:6)
#      expect_message(check_and_convert(df), "Input is a data frame.")
#    }
#  )
#})

# Additional tests can be added to check the functionality under different conditions.
