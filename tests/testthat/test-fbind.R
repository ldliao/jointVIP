test_that("fbind() binds factor (or character)", {
  x <- c("a", "b")
  x_fact <- factor(x)
  y <- c("c", "d")
  z <- factor(c("a", "b", "c", "d"))

  testthat::expect_identical(fbind(x, y), z)
  testthat::expect_identical(fbind(x_fact, y), z)
})
