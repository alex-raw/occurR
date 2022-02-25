test_that("available_measures returns correctly", {
  expect_type(available_measures("disp"), "character")
  expect_type(available_measures("assoc"), "character")
  expect_true(all(c("ll", "mi", "gmean") %in% available_measures("assoc")))
  expect_true(all(c("dp", "dp.norm") %in% available_measures("disp")))
  expect_identical(available_measures("disp")[1], "range")
  expect_type(available_measures(), "list")
  expect_length(available_measures(), 2)
})


test_that("as_factor same as returns an unordered variant of base::factor", {
  x <- sample(letters, 50, replace = TRUE)
  expect_identical(levels(factor(x, unique(x))), levels(as_factor(x)))
  expect_identical(as.integer(factor(x, unique(x))), as.integer(as_factor(x)))
  expect_identical(as.factor(1:10), as_factor(1:10))
  expect_identical(as.factor(1:10), as_factor(1:10, 1:10))
  expect_identical(as.factor(1.1:10), as_factor(1.1:10, 1.1:10))
})

test_that("as_factor handles special values", {
  x <- c("a", NA, "a", NA, "b")
  expect_identical(as_factor(x), factor(x, unique(x), exclude = NULL))
  y <- c("a", NaN, "a", NaN, "b")
  expect_identical(as_factor(y), factor(y, unique(y), exclude = NULL))
  expect_null(factorcpp(NA, letters))
})


test_that("sum_by handles missing values", {
  x <- as_factor(c("a", NA, "a", NA, "b"))
  n <- c(1, 2, 3, 4, 5)
  u <- length(unique(x))

  nas <- list(numeric(0), NA, NaN, NULL)
  lapply(nas,     function(y) expect_error(sum_by(y, u, n)))
  lapply(nas,     function(y) expect_error(sum_by(x, y, n)))
  lapply(nas[-1], function(y) expect_error(sum_by(x, u, y)))

  expect_identical(sum_by(x, u, numeric(0)), numeric(0))
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
  expect_null(check_funs(sum))
  expect_error(check_funs("nope"))
  expect_error(check_funs("logl"))
})

test_that("count", {
  x <- c(letters, letters)
  expect_identical(count(x), count(factor(x)))
  expect_length(count(x), 1)
  expect_true(is.numeric(count(x)))
})
