test_that("reference is here", {
  expect_s3_class(toy_dist, "data.frame")
})

# test_that("distances returns expected columns", {
#   expect_s3_class(ans1, "data.frame")
#   expect_setequal(colnames(ans1), c("types", "f", every))
#   expect_true(nrow(ans1) == length(unique(tokens)))
# })

# test_that("distances produces expected values, character input", {
#   expect_equal(ans1, toy_reference)
#   expect_equal(ans2, brown_reference)
# })
