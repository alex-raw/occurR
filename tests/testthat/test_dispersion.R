every <- Filter(\(x) x != "kld.norm", available_measures("disp"))
.read_table <- function(path, ...)
  utils::read.table(path, sep = "\t", na = "", quote = "", ..., header = TRUE)

data(brown)
x <- table(brown[, c("id", "word")]) |>
  as.data.frame(responseName = "v") |>
  with(dispersion(word, id, v, fun = every))

ref <- .read_table(
  "test_data_dispersion.tsv"
)[, colnames(x)]

x <- x[order(x$types), ][1:100, ]
ref <- ref[order(ref$types), ][1:100, ]


test_that("data sets are here", {
  expect_s3_class(reference_disp, "data.frame")
  expect_s3_class(brown, "data.frame")
  expect_identical(colnames(x), c("types", "f", every))
})

test_that("values are consistent with Gries", {
  expect_equal(x, ref, ignore_attr = TRUE)
})

test_that("minimal dp == dispersion(..., fun = \"dp\")", {
  n <- 50
  v <- sample(1:100, n, replace = TRUE)
  tokens <- sample(letters, n, replace = TRUE)
  parts <- sample(LETTERS[1:3], n, replace = TRUE)

  big <- dispersion(v, tokens, parts, "dp")
  big <- big[order(big$types), ]
  big <- setNames(big$dp, big$types)
  mini <- dp(v, tokens, parts, norm = FALSE)
  mini <- mini[order(names(mini))]
  expect_equal(mini, big)

  big <- dispersion(v, tokens, parts, "dp.norm")
  big <- big[order(big$types), ]
  big <- setNames(big$dp, big$types)
  mini <- dp(v, tokens, parts, norm = TRUE)
  mini <- mini[order(names(mini))]
  expect_equal(mini, big)
})
