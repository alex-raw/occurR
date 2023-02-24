missing_count <- data.frame(seq_along(letters))
missing_type <- data.frame(letters)
ambig_count <- data.frame(letters)
ambig_type <- data.frame(1:10, 1:10, letters[1:10])
case <- data.frame(seq_along(c(letters, LETTERS)), c(letters, LETTERS))
missing <- data.frame(c("a", "b", "  ", "c", ""), 1:5)
case_missing <- data.frame(c("a", "B", "b", "  ", "c", ""), 1:6)

test_that("check frequency list fails", {
  expect_error(check_frequency_list(list(1:3, letters[1:4])))
  expect_error(check_frequency_list(missing_count))
  expect_error(check_frequency_list(missing_type))
  expect_error(check_frequency_list(ambig_type))
  expect_error(check_frequency_list(ambig_count))
})

test_that("check frequency list warns", {
  expect_warning(check_frequency_list(case))
  expect_null(check_frequency_list(case, case_sensitive = TRUE))
  expect_warning(check_frequency_list(missing))
  expect_warning(check_frequency_list(case_missing))
})
