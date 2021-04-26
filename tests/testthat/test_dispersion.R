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
