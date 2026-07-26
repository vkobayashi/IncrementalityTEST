example_results <- data.frame(
  experiment = rep(c("A", "B", "C"), each = 2),
  group = rep(c("control", "treatment"), 3),
  RPU = c(10, 11, 8, 9.5, 12, 11.5)
)

test_that("metric_differences calculates treatment minus control", {
  result <- metric_differences(example_results, "RPU")
  expect_named(result, c("experiment", "control", "treatment", "difference"))
  expect_equal(result$difference, c(1, 1.5, -0.5))
})

test_that("direction can be reversed", {
  result <- metric_differences(
    example_results, "RPU", direction = "control-treatment"
  )
  expect_equal(result$difference, c(-1, -1.5, 0.5))
})

test_that("invalid experimental structures are rejected", {
  duplicated_row <- rbind(example_results, example_results[1, ])
  expect_error(metric_differences(duplicated_row, "RPU"), "exactly one")

  incomplete <- example_results[-2, ]
  expect_error(metric_differences(incomplete, "RPU"), "Every experiment")
  expect_equal(
    nrow(metric_differences(incomplete, "RPU", na_action = "omit")),
    2
  )
})

test_that("legacy test_metric follows original sign convention", {
  legacy <- data.frame(
    iabtest_id = rep(1:2, each = 2),
    abt_group = rep(0:1, 2),
    RPU = c(10, 11, 8, 9)
  )
  result <- test_metric(legacy, "RPU")
  expect_named(result, c("iabtest_id", "inc_RPU"))
  expect_equal(result$inc_RPU, c(-1, -1))
})
