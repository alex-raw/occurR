test_that("assoc builtins", {
  expect_type(builtin_assoc(), "expression")
  result <- eval(builtin_assoc()["ll"], list(
    o = matrix(11:20, 2),
    e = matrix(1:10, 2)
  ))
  expect_true(is.numeric(result))
  expect_length(result, 2)
})

test_that("correct starting values", {
  expect_true(is.numeric(get_obs(10, 10, 1, 10)))
  expect_true(all(get_obs(0, 0, 0, 0) == 0))
  expect_true(all(is.na(get_obs(10, 10, NA, 10))))
  expect_equal(c(get_obs(10, 10, 1, 20)), c(1, 9, 9, 1))

  expect_true(is.numeric(get_exp(1, 2, 3)))
  expect_true(all(is.nan(get_exp(0, 0, 0))))
  expect_true(all(is.na(get_exp(NA, 120, 1000))))
  expect_equal(c(get_exp(100, 75, 300)), c(25, 50, 75, 150))
})

test_that("variables are retrieved correctly", {
  expect_error(get_assoc_vars("nope", list(f1 = 10)))
  expect_error(get_assoc_vars("c1", list(f1 = 10)))
  expect_identical(get_assoc_vars("r1", list(f1 = 10)), 10)
})

test_that("modular values are same as individual ones", {
  vals <- list(f1 = 10, f2 = 11, o11 = 1, n = 21)
  expect_equal(
    sapply(c("e11", "e12", "e21", "e22"), get_assoc_vars, vals),
    c(get_exp(10, 11, 21)),
    ignore_attr = TRUE
  )
  expect_equal(
    sapply(c("o11", "o12", "o21", "o22"), get_assoc_vars, vals),
    c(get_obs(10, 11, 1, 21)),
    ignore_attr = TRUE
  )
})

test_that("bare-bones functions return same as main functions", {
  o11 <- sample(1:100, 20)
  f1 <- o11 + sample(1:100, 20)
  expect_identical(ll(f1, o11, one_sided = FALSE),
    c(coll(f1, o11, fun = "ll")),
    ignore_attr = FALSE
  )
})

test_that("doesn't allow o11 > f1/f2", {
  expect_error(coll(1, 2, 1, builtin_assoc()))
  expect_error(coll(1, 2, 2, builtin_assoc()))
  expect_error(coll(2, 2, 2, builtin_assoc()))
})

test_that("doesn't allow corpus size `n` to be too small", {
  expect_error(coll(2, 0, 1, 2, builtin_assoc()))
  expect_error(coll(1, 0, 2, 2, builtin_assoc()))
  expect_error(coll(2, 2, 2, 3, builtin_assoc()))
})

test_that("lengths", {
  expect_error(coll(c(12, 13), 0, 12, 12, builtin_assoc()))
  expect_error(coll(1:4, 1:4, 20, numeric(0), builtin_assoc()))
  expect_error(coll(1:4, 1:4, numeric(0), 20, builtin_assoc()))
  expect_error(coll(1:4, numeric(0), 20, 20, builtin_assoc()))
  expect_error(coll(numeric(0), 1:4, 20, 20, builtin_assoc()))
  expect_length(coll(c(0:3), c(0:3)), 4)
})

test_that("custom functions", {
  expect_equal(2, coll(2, 1, 3, 10, expression(f1)))
  expect_equal(1, coll(2, 1, 3, 10, expression(o11)))
  expect_equal(3, coll(2, 1, 3, 10, expression(f2)))
  expect_equal(5, coll(2, 1, 3, fun = expression(n)))
  expect_equal(
    coll(2, 1, 3, 10),
    coll(2, 1, 3, 10,
         expression(2 * rowSums(o * log(o / e), na.rm = TRUE))),
    ignore_attr = TRUE
  )
})

test_that("colnames", {
  expect_equal(
    c("ll", "mi"),
    colnames(coll(1:2, 0:1, 3, 10, c("ll", "mi")))
  )
  expect_equal(
    "custom",
    colnames(coll(1:2, 0:1, 3, 10, expression(custom = log(o11 / o22))))
  )
})
