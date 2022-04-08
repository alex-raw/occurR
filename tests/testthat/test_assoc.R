assocs <- builtin_assoc()
to_remove <- c("zscore", "zscore_cor", "mi_conf", "poisson_pv")
assocs <- assocs[!names(assocs) %in% to_remove]

test_that("assoc builtins", {
  expect_type(assocs, "expression")
  result <- eval(assocs["ll"], list(
    o = matrix(11:20, 2),
    e = matrix(1:10, 2)
  ))
  expect_true(is.numeric(result))
  expect_length(result, 2)
})

test_that("bare-bones functions return same as main functions", {
  o11 <- sample(1:100, 20)
  f1 <- o11 + sample(1:100, 20)
  expect_identical(ll(o11, f1, one_sided = FALSE),
    c(coll(o11, f1, fun = "ll")),
    ignore_attr = FALSE
  )
})

test_that("doesn't allow o11 > f1/f2", {
  expect_error(coll(2, 1, 1, assocs))
  expect_error(coll(2, 1, 2, assocs))
  expect_error(coll(2, 2, 2, assocs))
})

test_that("doesn't allow corpus size `n` to be too small", {
  expect_error(coll(0, 2, 1, 2, assocs))
  expect_error(coll(0, 1, 2, 2, assocs))
  expect_error(coll(2, 2, 2, 3, assocs))
})

test_that("lengths", {
  expect_error(coll(0, c(12, 13), 12, 12, assocs))
  expect_error(coll(1:4, 1:4, 20, numeric(0), assocs))
  expect_error(coll(1:4, 1:4, numeric(0), 20, assocs))
  expect_error(coll(1:4, numeric(0), 20, 20, assocs))
  expect_error(coll(numeric(0), 1:4, 20, 20, assocs))
  expect_length(coll(c(0:3), c(0:3)), 4)
})

test_that("custom functions", {
  m <- \(n, name) matrix(n, dimnames = list(NULL, name))
  expect_equal(coll(1, 2, 3, 10, expression(f1 = f1)), m(2, "f1"))
  expect_equal(coll(1, 2, 3, 10, expression(o11 = o11)), m(1, "o11"))
  expect_equal(coll(1, 2, 3, 10, expression(f2 = f2)), m(3, "f2"))
  expect_equal(coll(1, 2, 3, fun = expression(n = n)), m(5, "n"))
  expect_equal(
    coll(1, 2, 3, 10, expression(ll2 = 2 * rowSums(o * log(o / e), na.rm = TRUE))),
    coll(1, 2, 3, 10),
    ignore_attr = TRUE
  )
})

test_that("colnames", {
  expect_equal(
    colnames(coll(0:1, 1:2, 3, 10, c("ll", "mi"))),
    c("ll", "mi")
  )
  expect_equal(
    colnames(coll(0:1, 1:2, 3, 10, expression(custom = log(o11 / o22)))),
    "custom"
  )
})

test_that("flip", {
  x <- coll(1, 100, 1000, fun = assocs)
  y <- coll(1, 100, 1000, fun = assocs, flip = "ll")
  y[, "ll"] <- -y[, "ll"]
  z <- coll(1, 100, 1000, fun = assocs, flip = "")
  w <- coll(1, 100, 1000, fun = assocs, flip = c("ll", "jaccard"))
  w[, "ll"] <- -w[, "ll"]
  w[, "jaccard"] <- -w[, "jaccard"]
  expect_identical(x, z)
  expect_identical(x, y)
  expect_identical(x, y)

  custom_noflip <- coll(1, 1000, 1000, fun = expression(test = e12))
  custom_flip <- coll(1, 1000, 1000, fun = expression(test = e12), flip = "test")
  custom_flip[, "test"] <- -custom_flip[, "test"]
  expect_identical(custom_flip, custom_noflip)
})
