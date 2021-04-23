every <- available_measures("disp")
v <- freq_list$Freq
tokens <- freq_list$tokens
parts <- freq_list$parts
ans <- dispersion(v, tokens, parts, every)

test_that("Reference data is here", {
  expect_true(is.data.frame(freq_list))
})

test_that("dispersion returns expected columns", {
  expect_s3_class(ans, "data.frame")
  expect_setequal(colnames(ans), c("types", "f", every))
  expect_true(nrow(ans) == length(unique(tokens)))
})
