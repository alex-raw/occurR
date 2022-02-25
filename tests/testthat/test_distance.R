test_dwg <- function(corr = TRUE) {
  i <- seq(1, 100, 2)
  ans1 <- dwg(i, length(i), 100L, corr)
  expect_vector(ans1, numeric())
  expect_gt(ans1, 0)
  expect_lt(ans1, 1)

  i <- 1:10
  ans2 <- dwg(i, length(i), 100L, corr)
  expect_vector(ans2, numeric())
  expect_lt(ans1, ans2)

  ans3 <- distance(i, length(i), 100L, corr)
  expect_identical(ans2, ans3)
}

test_that("dwg", {
  test_dwg(TRUE)
  test_dwg(FALSE)
})
