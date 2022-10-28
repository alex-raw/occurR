measures <- builtin_assoc()
ref_data <- utils::read.table("test_data_collex.tsv",
                         sep = "\t", na = "", quote = "", header = TRUE)

m <- c("f1", "f2", "o11", "o22", "o12", "o21", "e11", available_measures("assoc"))
suppressWarnings({
  test_data <- collexemes(ref_data, o11, f1, n = 1e8L, fun = m)
})

implemented <- intersect(colnames(ref_data), colnames(test_data))
test_data <- test_data[, implemented]
ref_data <- ref_data[order(test_data$type), implemented]
test_data$e11 <- round(test_data$e11, 1) # bug in collostructions

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

test_that("doesn't allow corpus size `n` to be too small", {
  expect_error(coll(0, 2, 1, 2, measures))
  expect_error(coll(0, 1, 2, 2, measures))
  expect_error(coll(2, 2, 2, 3, measures))
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

test_that("values consistent with Flach's `collostructions`", {
  expect_equal(test_data[, implemented], ref_data[, implemented])
})

cols <- list(
  type = "character",
  o11 = "numeric",
  other = NULL,
  relation = NULL,
  ll = "numeric",
  pearsonsid = "numeric",
  odds = "numeric",
  mi_base2 = "numeric",
  delta_p2 = "numeric", # TODO: figure out which way around to name these
  delta_p1 = "numeric",
  fye = "numeric"
)

# last accessed 2022-10-28 18:26
# script <- utils::download.file(con = "https://www.stgries.info/teaching/groningen/coll.analysis.r", "gries_coll_4.0.R")
gries_output <- "https://www.stgries.info/teaching/groningen/1_out.csv" |>
  read.table(header = TRUE, col.names = names(cols), colClasses = cols)

ref_data_gries <- "https://www.stgries.info/teaching/groningen/1.csv" |>
  read.table(header = TRUE, col.names = c("type", "f1", "o11")) |>
  merge(gries_output)

n <- 138664

# TODO: consider doing this in the package
input <- rbind(
  ref_data_gries[, c("type", "o11", "f1")],
  data.frame(type = "OTHER", o11 = 0, f1 = n - sum(ref_data_gries$o11))
)

test_data_gries <- collexemes(input, o11, f1, n = n, fun = m)[-nrow(input), ]

implemented <- intersect(available_measures("assoc"), colnames(ref_data_gries))

test_that("values consistent with Gries's `Coll.analysis 4.0`", {
  expect_equal(test_data_gries[, implemented], ref_data_gries[, implemented])
})

