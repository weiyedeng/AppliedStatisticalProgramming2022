context("Get the length of the updated weights correctly")

y <- c(0.1,0.2,0.5)
ftk <- seq(0,1,0.1)
sd <- 1
weights <- rep(1/length(ftk), length(ftk)) ## Initially assign equal weight to each model

test_that("correct length of the updated weights", {
  expect_equal(length(update_weights(est_ztk(y, ftk, sd, weights))),
              11)
})