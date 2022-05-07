context("Get the dimensions of the numerator matrix and the length of the denominator vector correctly")

y <- c(0.1,0.2,0.5)
ftk <- seq(0,1,0.1)
sd <- 1
weights <- rep(1/length(ftk), length(ftk)) ## Initially assign equal weight to each model

test_that("correct dimensions of the numerator and correct length of the denominator", {
  expect_equal(dim(get_num_den(y, ftk, sd, weights)[[1]]),
              c(3,11))
  expect_equal(length(get_num_den(y, ftk, sd, weights)[[2]]),
               3)
})