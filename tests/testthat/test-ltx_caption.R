library(testthat)
# Dummy LaTeX table data to be used in the tests
tbl_code <- c("\\usepackage{booktabs}",
              "\\caption{Sample Table}",
              "\\label{tab:example}",
              "\\begin{tabular}{|c|c|}",
              "\\hline",
              "A & B \\\\",
              "\\hline",
              "1 & 2 \\\\",
              "\\hline",
              "\\end{tabular}")

# Test cases for ltx_header function
test_that("ltx_header processes LaTeX table without errors", {
    result <- ltx_caption(tbl_code, tbl_note = "", print_tbl = FALSE)
    expect_type(result, "character")
    expect_true(any(stringr::str_detect(result, "\\\\caption")))
    expect_false(any(stringr::str_detect(result, "\\\\usepackage")))  # Should remove \\usepackage lines
    expect_false(any(stringi::stri_isempty(result)))  # No empty lines
})

test_that("ltx_header correctly handles table note", {
    result <- ltx_caption(tbl_code, tbl_note = "Table Note", print_tbl = FALSE)
    expect_true(any(stringr::str_detect(result, "Table Note")))  # Note should be included
    expect_true(any(stringr::str_detect(result, "\\\\header\\{Sample Table")))  # Header should be inserted
})

test_that("ltx_header correctly processes table without label", {
    tbl_code_no_label <- c("\\usepackage{booktabs}",
                           "\\caption{Sample Table}",
                           "\\begin{tabular}{|c|c|}",
                           "\\hline",
                           "A & B \\\\",
                           "\\hline",
                           "1 & 2 \\\\",
                           "\\hline",
                           "\\end{tabular}")

    result <- ltx_caption(tbl_code_no_label, tbl_note = "", print_tbl = FALSE)
    expect_true("\\caption{\\header{Sample Table}}" %in% result)
    expect_false("\\label" %in% result)  # No label should be present
})

test_that("ltx_header correctly processes table with label", {
    result <- ltx_caption(tbl_code, tbl_note = "", print_tbl = FALSE)
    expect_true(any(stringr::str_detect(result, "\\label\\{tab:example\\}")))
    expect_true(any(stringr::str_detect(result, "\\\\header\\{Sample Table\\}\\}")))
})

test_that("ltx_header correctly prints the table when print_tbl is TRUE", {
    expect_output(ltx_caption(tbl_code, tbl_note = "", print_tbl = TRUE), "")  # Check console output
})
