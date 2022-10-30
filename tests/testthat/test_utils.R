test_that("available_measures returns correctly", {
  expect_type(available_measures("disp"), "character")
  expect_type(available_measures("assoc"), "character")
  expect_true(all(c("ll", "mi", "gmean") %in% available_measures("assoc")))
  expect_true(all(c("dp", "dp_norm") %in% available_measures("disp")))
  expect_identical(available_measures("disp")[1], "range")
  expect_type(available_measures(), "list")
  expect_length(available_measures(), 2)
})

test_that("sum_by handles missing values", {
  x <- as_factor(c("a", "s", NA, "s", "c"))
  n <- c(1, 2, 3, 4, 5)
  u <- length(unique(x))

  nas <- list(numeric(0), NA, NaN, NULL)
  lapply(nas,     \(y) expect_error(sum_by(y, u, n)))
  lapply(nas,     \(y) expect_error(sum_by(x, y, n)))
  lapply(nas[-1], \(y) expect_error(sum_by(x, u, y)))

  expect_error(sum_by(x, u, numeric(0)))
  expect_identical(sum_by(x, u, n), as.vector(rowsum(n, x)))

  x <- as_factor(c("a", "c", "a", "c", "b"))
  n <- c(1, 2, NA, 4, 5)
  u <- length(unique(x))
  expect_identical(sum_by(x, u, n), as.vector(rowsum(n, x)))

  x <- as_factor(c("", "c", "a", "c", "b"))
  n <- c(1, 2, NA, 4, 5)
  u <- length(unique(x))
  expect_identical(sum_by(x, u, n), as.vector(rowsum(n, x)))
})

test_that("incompatible functions throw error", {
  expect_error(check_funs("nope", builtin_assoc()))
  expect_error(check_funs("logl", builtin_assoc()))
})
