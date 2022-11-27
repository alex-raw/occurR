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
    c(assoc(o11, f1, fun = "ll")),
    ignore_attr = FALSE
  )
})

test_that("doesn't allow o11 > f1/f2", {
  expect_error(coll_analysis.default(2, 1, 1, measures))
  expect_error(coll_analysis.default(2, 1, 2, measures))
  expect_error(coll_analysis.default(2, 2, 2, measures))
})

test_that("lengths", {
  expect_error(coll_analysis.default(NULL, 0, c(12, 13), 12, 12, measures))
  expect_error(coll_analysis.default(NULL, 1:4, 1:4, 20, numeric(0), measures))
  expect_error(coll_analysis.default(NULL, 1:4, 1:4, numeric(0), 20, measures))
  expect_error(coll_analysis.default(NULL, 1:4, numeric(0), 20, 20, measures))
  expect_error(coll_analysis.default(NULL, numeric(0), 1:4, 20, 20, measures))
  expect_length(coll_analysis.default(NULL, c(0:3), c(0:3)), 4)
})

test_that("custom output columns", {
  mat <- \(n, name) matrix(n, dimnames = list(NULL, name))
  assoc(1, 2, 3, 10, fun = list(corpus_freq = "f1")) |>
    expect_equal(mat(2, c("corpus_freq")))
  assoc(1, 2, 3, 10, fun = list(
    ll_custom = \() 2 * rowSums(o * log(o / e), na.rm = TRUE)
  )) |>
    expect_equal(assoc(1, 2, 3, 10), ignore_attr = TRUE)
})

test_that("colnames", {
  colnames(assoc(0:1, 1:2, 3, 10, fun = c("ll", "mi"))) |>
    expect_equal(c("ll", "mi"))
  colnames(assoc(0:1, 1:2, 3, 10, fun = list(custom = \() log(o11 / o22)))) |>
    expect_equal("custom")
})

test_that("flip", {
  measures <- available_measures("assoc")
  x <- assoc(1, 100, 1000, fun = measures)
  y <- assoc(1, 100, 1000, fun = measures, flip = "ll")
  y[, "ll"] <- -y[, "ll"]
  z <- assoc(1, 100, 1000, fun = measures, flip = "")
  w <- assoc(1, 100, 1000, fun = measures, flip = c("ll", "jaccard"))
  w[, "ll"] <- -w[, "ll"]
  w[, "jaccard"] <- -w[, "jaccard"]
  expect_equal(x, z)
  expect_equal(x, y)
  expect_equal(x, w)

  custom_noflip <- assoc(1, 1000, 1000, fun = list(test = "e12"))
  custom_flip <- assoc(1, 1000, 1000, fun = list(test = "e12"), flip = "test")
  custom_flip[, "test"] <- -custom_flip[, "test"]
  expect_identical(custom_flip, custom_noflip)
})


ref_data <- .read_table("test_data_collex.tsv")
m <- c("f1", "f2", "o11", "o22", "o12", "o21", "e11",
       available_measures("assoc"))

test_that("values consistent with Flach's `collostructions`", {
  test_data <- coll_analysis(ref_data, o11, f1, n = 1e8L, fun = m) |>
    suppressWarnings()
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
    data.frame(type = "__OTHER__", o11 = 0, f1 = n - sum(ref_data_gries$o11))
  )
  test_data_gries <- coll_analysis(input, o11, f1, n = n, fun = m)
  implemented <- intersect(
    available_measures("assoc"),
    colnames(ref_data_gries)
  )

  expect_equal(
    test_data_gries[-nrow(test_data_gries), implemented],
    ref_data_gries[, implemented]
  )

  mi_base2 <- \(o11, e11) log2(o11 / e11)
  res_mi_base2 <- coll_analysis(input, o11, f1, n = n, fun = mi_base2)

  expect_equal(
    res_mi_base2[-nrow(input), ],
    ref_data_gries[, c("type", "mi_base2")]
  )
})

test_that("fye is consistent with `fisher.test`", {
  cxn <- sum(ref_data$o11)
  mat <- matrix(c(280, cxn - 280, 67965 - 280, 1e8 - (67965 + (cxn - 280))), 2)
  pkg_fye <- c(assoc(280, 67965, cxn, fun = "fye", n = 1e8))
  r_fye <- -log10(fisher.test(mat, alternative = "greater")$p.value)

  # # Flach accumulates rounding error? this is not equal:
  # collostructions::collex(
  #   data.frame(c("DUMMY", "test"), 279:280, 67964:67965),
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

.x <- data.frame(
  word = letters,
  collocate = rev(letters),
  o11 = o11 <- sample.int(100, length(letters)),
  f1 = f1 <- o11 + sample.int(100, length(letters)),
  f2 = f1 + sample.int(100, length(letters)),
  n = 1e6
)

ref_res <- coll_analysis(.x)

test_that("same result if column names match args names", {
  expect_identical(
    ref_res,
    coll_analysis(.x, o11 = o11, f1 = f1, f2 = f2, n = n)
  )
})

test_that("explicit args take precedence over implicit columns", {
  expect_identical(
    coll_analysis(.x, n = 1e8),
    coll_analysis(.x, o11 = o11, f1 = f1, f2 = f2, n = 1e8)
  )
  expect_identical(
    coll_analysis(.x, f2 = 1e5),
    coll_analysis(.x, o11 = o11, f1 = f1, f2 = 1e5, n = n)
  )
  expect_identical(
    coll_analysis(.x, f2 = 1e5, n = 1e7),
    coll_analysis(.x, o11 = o11, f1 = f1, f2 = 1e5, n = 1e7)
  )
})

test_that("error when columns are missing", {
  subset(.x, select = c(word, o11)) |>
    coll_analysis() |>
    expect_error("at least o11 and f1")
})

test_that("column order doesn't matter if columns named accordingly", {
  .y <- data.frame(
    word = letters,
    f1 = f1 <- o11 + sample.int(100, length(letters)),
    collocate = rev(letters),
    f2 = f1 + sample.int(100, length(letters)),
    n = 1e6,
    o11 = o11 <- sample.int(100, length(letters))
  )
  expect_identical(ref_res, coll_analysis(.x))
})

test_that("error when names missing results if names missing", {
  coll_analysis(stats::setNames(.x, NULL)) |>
    expect_error("missing columns")
  coll_analysis(stats::setNames(.x, c("word", "collocate", "nope", "f1", "f2", "n"))) |>
    expect_error("missing columns")
})

test_that("mixed fun list works", {
  mi_base2 <- \(o11, e11) log2(o11 / e11)
  coll_analysis(.x, o11, f1, n = n, fun = list(
    builtin = "ll",
    `function` = mi_base2,
    anonymous = \(e11) e11^2,
    `expression` = expression(o11 + o12)
  )) |> sapply(is.numeric) |> sum() |> expect_identical(4L)
})

test_that("matrix input works", {
  .m <- as.matrix(.x[3:6])
  rownames(.m) <- paste(.x$word, .x$collocate, sep = "-")
  expect_identical(
    coll_analysis(.x)$ll,
    coll_analysis(.m)[, "ll"],
    ignore_attr = TRUE
  )
  expect_identical(
    coll_analysis(.x, n = 1e4L)$ll,
    coll_analysis(.m, n = 1e4L)[, "ll"],
    ignore_attr = TRUE
  )
  expect_identical(
    coll_analysis(.x, f2 = 1e4L)$ll,
    coll_analysis(.m, f2 = 1e4L)[, "ll"],
    ignore_attr = TRUE
  )
  expect_identical(
    coll_analysis(.x, fun = c("ll", "mi"))[, c("ll", "mi")] |> as.matrix(),
    coll_analysis(.m, fun = c("ll", "mi"))[, c("ll", "mi")],
    ignore_attr = TRUE
  )
})

test_that("unknown character funs throw error", {
  expect_error(
    coll_analysis(.x, fun = c("logl", "mi")),
    "No built-in"
  )
})

test_that("missing names in list throw error", {
  expect_error(
    coll_analysis(.x, fun = list(logl = "ll", "mi")),
    "have to be named"
  )
})
