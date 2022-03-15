test_that("dispersion returns expected columns", {
  expect_s3_class(ans1, "data.frame")
  expect_setequal(colnames(ans1), c("types", "f", every))
  expect_true(nrow(ans1) == length(unique(tokens)))
})

test_that("dispersion produces expected values, character input", {
  expect_equal(ans1, toy_reference)
  expect_equal(ans2, brown_reference)
})

test_that("dispersion produces expected values, factor input", {
  expect_equal(ans1_fact, toy_reference)
  expect_equal(ans2_fact, brown_reference)
})

test_that("minimal dp == dispersion(..., fun = \"dp\")", {
  n <- 50
  v <- sample(1:100, n, replace = TRUE)
  tokens <- sample(letters, n, replace = TRUE)
  parts <- sample(LETTERS[1:3], n, replace = TRUE)

  expect_equal(
    dp(v, tokens, parts),
    dispersion(v, tokens, parts)$dp.norm,
    ignore_attr = TRUE
  )

  expect_equal(
    dp(v, tokens, parts, norm = FALSE),
    dispersion(v, tokens, parts, "dp")$dp,
    ignore_attr = TRUE
  )
})
