test_that("dwg", {
  i <- seq(1, 100, 2)
  ans1 <- dwg(i, length(i), 100L)
  expect_vector(ans1, numeric())
  expect_gt(ans1, 0)
  expect_lt(ans1, 1)

  i <- 1:10
  ans2 <- dwg(i, length(i), 100L)
  expect_vector(ans2, numeric())
  expect_lt(ans1, ans2)
})
