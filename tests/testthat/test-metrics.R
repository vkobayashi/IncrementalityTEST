test_that("calculate_metrics computes expected metrics", {
  result <- calculate_metrics(10, 100, 250, 5)
  expect_s3_class(result, "data.frame")
  expect_equal(result$RPU, 2.5)
  expect_equal(result$BR, 0.05)
  expect_equal(result$AOV, 25)
  expect_equal(result$TPB, 2)
})

test_that("calculate_metrics supports vectors and safe zero division", {
  result <- calculate_metrics(c(10, 0), c(100, 0), c(250, 0), c(5, 0))
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result[2, ])))
})

test_that("calculate_metrics validates inputs", {
  expect_error(calculate_metrics("10", 100, 250, 5), "numeric")
  expect_error(calculate_metrics(-1, 100, 250, 5), "negative")
})

test_that("legacy metric interface returns a named list", {
  result <- incrementality_metrics(10, 100, 250, 5)
  expect_type(result, "list")
  expect_named(result, c("RPU", "BR", "AOV", "TPB"))
})
