test_that("t interval agrees with stats t.test", {
  x <- c(0.4, 0.8, 0.1, 0.6, 0.9)
  result <- t_confidence_interval(x)
  expected <- t.test(x)$conf.int
  expect_equal(result$mean, mean(x))
  expect_equal(c(result$lower, result$upper), as.numeric(expected))
})

test_that("t interval validates its arguments", {
  expect_error(t_confidence_interval(1), "At least two")
  expect_error(t_confidence_interval(c(1, 2), 1), "strictly between")
  expect_error(t_confidence_interval(c(1, NA), na_rm = FALSE), "missing")
})

test_that("bootstrap is reproducible and preserves caller RNG state", {
  set.seed(99)
  before <- .Random.seed
  first <- bootstrap_incrementality(1:5, times = 100, seed = 42)
  after <- .Random.seed
  second <- bootstrap_incrementality(1:5, times = 100, seed = 42)

  expect_equal(before, after)
  expect_equal(first$replicates, second$replicates)
  expect_lte(first$lower, first$estimate)
  expect_gte(first$upper, first$estimate)
})

test_that("legacy t interval uses alpha", {
  data <- data.frame(id = 1:5, difference = c(0.4, 0.8, 0.1, 0.6, 0.9))
  result <- t_test_cf(data, 0.05)
  expect_equal(result$confidence_interval, as.numeric(t.test(data$difference)$conf.int))
})
