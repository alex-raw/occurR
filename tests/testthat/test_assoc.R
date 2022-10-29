.read_table <- \(...) utils::read.table(..., sep = "\t", na = "", quote = "",
                                        header = TRUE)
measures <- builtin_assoc()


test_that("assoc builtins", {
  expect_type(measures, "expression")
  result <- eval(measures["ll"], list(
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
  expect_error(coll(2, 1, 1, measures))
  expect_error(coll(2, 1, 2, measures))
  expect_error(coll(2, 2, 2, measures))
})

test_that("lengths", {
  expect_error(coll(0, c(12, 13), 12, 12, measures))
  expect_error(coll(1:4, 1:4, 20, numeric(0), measures))
  expect_error(coll(1:4, 1:4, numeric(0), 20, measures))
  expect_error(coll(1:4, numeric(0), 20, 20, measures))
  expect_error(coll(numeric(0), 1:4, 20, 20, measures))
  expect_length(coll(c(0:3), c(0:3)), 4)
})

test_that("custom functions", {
  mat <- \(n, name) matrix(n, dimnames = list(NULL, name))
  expect_equal(coll(1, 2, 3, 10, expression(f1 = f1)), mat(2, "f1"))
  expect_equal(coll(1, 2, 3, 10, expression(o11 = o11)), mat(1, "o11"))
  expect_equal(coll(1, 2, 3, 10, expression(f2 = f2)), mat(3, "f2"))
  expect_equal(coll(1, 2, 3, fun = expression(n = n)), mat(5, "n"))
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
  x <- coll(1, 100, 1000, fun = measures)
  y <- coll(1, 100, 1000, fun = measures, flip = "ll")
  y[, "ll"] <- -y[, "ll"]
  z <- coll(1, 100, 1000, fun = measures, flip = "")
  w <- coll(1, 100, 1000, fun = measures, flip = c("ll", "jaccard"))
  w[, "ll"] <- -w[, "ll"]
  w[, "jaccard"] <- -w[, "jaccard"]
  expect_equal(x, z)
  expect_equal(x, y)
  expect_equal(x, w)

  custom_noflip <- coll(1, 1000, 1000, fun = expression(test = e12))
  custom_flip <- coll(1, 1000, 1000, fun = expression(test = e12), flip = "test")
  custom_flip[, "test"] <- -custom_flip[, "test"]
  expect_identical(custom_flip, custom_noflip)
})

test_that("no integer overflow", {
  expect_warning(coll(1e8L, 1e9L))
})


ref_data <- .read_table("test_data_collex.tsv")
m <- c("f1", "f2", "o11", "o22", "o12", "o21", "e11", available_measures("assoc"))

test_that("values consistent with Flach's `collostructions`", {
  test_data <- collexemes(ref_data, o11, f1, n = 1e8L, fun = m) |> suppressWarnings()
  implemented <- intersect(colnames(ref_data), colnames(test_data))
  test_data <- test_data[, implemented]
  ref_data <- ref_data[order(test_data$type), implemented]
  test_data$e11 <- round(test_data$e11, 1) # bug in collostructions
  expect_equal(test_data[, implemented], ref_data[, implemented],
    tolerance = 1.5e-04)
})


test_that("values consistent with Gries's `Coll.analysis 4.0`", {
  ref_data_gries <- .read_table("test_data_gries4.tsv")
  n <- 138664  # see https://www.stgries.info/teaching/groningen/readme.txt
  input <- rbind(  # TODO: consider doing this in the package
    ref_data_gries[, c("type", "o11", "f1")],
    data.frame(type = "OTHER", o11 = 0, f1 = n - sum(ref_data_gries$o11))
  )
  test_data_gries <- collexemes(input, o11, f1, n = n, fun = m)

  implemented <- intersect(available_measures("assoc"), colnames(ref_data_gries))

  expect_equal(
    test_data_gries[-nrow(test_data_gries), implemented],
    ref_data_gries[, implemented]
  )

  fun <- expression(mi_base2 = log2(o11 / e11))  # TODO: to example
  mi_base2 <- collexemes(input, o11, f1, n = n, fun = fun)[-nrow(input), ]

  expect_equal(mi_base2, ref_data_gries[, c("type", "mi_base2")])
})

test_that("fye is consistent with `fisher.test`", {
  cxn <- sum(ref_data$o11)
  mat <- matrix(c(280, cxn - 280, 67965 - 280, 1e8 - (67965 + (cxn - 280))), 2)
  pkg_fye <- c(coll(280, 67965, cxn, fun = "fye", n = 1e8))
  r_fye <- -log10(fisher.test(mat, alternative = "greater")$p.value)

  # # Flach accumulates rounding error? this is not equal:
  # collostructions::collex(
  #   data.frame(c("DUMMY", "test"), 280:281, 67965:67966),
  #   cxn.freq = sum(ref_data$o11),
  #   am = "fye"
  # )$COLL.STR.FYE[2] |> suppressMessages()

  # # Gries mpfr implementation produces negative values? not equal:
  # expect_equal(
  #   test_data_gries[, c("type", "fye")],
  #   ref_data_gries[, c("type", "fye")]
  # )

  expect_identical(pkg_fye, r_fye)
})
