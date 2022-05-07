context("Get the dimensions of the ztk matrix correctly")

y <- c(0.1,0.2,0.5)
ftk <- seq(0,1,0.1)
sd <- 1
weights <- rep(1/length(ftk), length(ftk)) ## Initially assign equal weight to each model

test_that("correct dimensions of ztk matrix", {
  expect_equal(dim(est_ztk(y, ftk, sd, weights)),
              c(3,11))
})