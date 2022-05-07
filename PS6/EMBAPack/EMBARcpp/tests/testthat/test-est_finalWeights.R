context("Get the length of the final weights correctly")

y <- c(0.1,0.2,0.5)
ftk <- seq(0,1,0.1)
sd <- 1
weights <- rep(1/length(ftk), length(ftk)) ## Initially assign equal weight to each model
threshold <- 0.0001

test_that("correct dimensions of the numerator and correct length of the denominator", {
  expect_equal(length(est_finalWeights(y, ftk, sd, weights, threshold)[[1]]),
               11)
})