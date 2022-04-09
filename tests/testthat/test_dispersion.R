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
