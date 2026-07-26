test_that("high-level analysis returns complete results", {
  data <- data.frame(
    experiment = rep(LETTERS[1:4], each = 2),
    group = rep(c("control", "treatment"), 4),
    metric = c(10, 11, 8, 9, 12, 13, 9, 10)
  )
  result <- analyze_incrementality(
    data, "metric", bootstrap_times = 99, seed = 1
  )

  expect_s3_class(result, "incrementality_analysis")
  expect_equal(nrow(result$differences), 4)
  expect_equal(result$t_interval$mean, 1)
  expect_equal(result$bootstrap$times, 99)
  expect_output(print(result), "Incrementality analysis")
})
