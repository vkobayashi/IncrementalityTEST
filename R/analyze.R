#' Analyze an incrementality experiment collection
#'
#' A high-level workflow that pairs groups, calculates experiment-level
#' differences, and summarizes the overall effect with t and bootstrap
#' confidence intervals.
#'
#' @inheritParams metric_differences
#' @param conf_level Confidence level used by both interval estimators.
#' @param bootstrap_times Number of bootstrap replicates.
#' @param seed Optional bootstrap seed.
#'
#' @return An object of class `incrementality_analysis`.
#' @export
#'
#' @examples
#' results <- data.frame(
#'   experiment = rep(LETTERS[1:4], each = 2),
#'   group = rep(c("control", "treatment"), 4),
#'   revenue_per_user = c(10, 11, 8, 8.8, 12, 13, 9, 10.5)
#' )
#' analyze_incrementality(
#'   results, "revenue_per_user",
#'   bootstrap_times = 199, seed = 42
#' )
analyze_incrementality <- function(data, metric, id_col = "experiment",
                                   group_col = "group", control = "control",
                                   treatment = "treatment",
                                   direction = c("treatment-control",
                                                 "control-treatment"),
                                   na_action = c("error", "omit"),
                                   conf_level = 0.95,
                                   bootstrap_times = 2000L,
                                   seed = NULL) {
  direction <- match.arg(direction)
  na_action <- match.arg(na_action)
  differences <- metric_differences(
    data, metric, id_col, group_col, control, treatment, direction, na_action
  )
  t_interval <- t_confidence_interval(differences$difference, conf_level)
  bootstrap <- bootstrap_incrementality(
    differences$difference, bootstrap_times, conf_level, seed = seed
  )
  structure(
    list(
      metric = metric,
      direction = direction,
      differences = differences,
      t_interval = t_interval,
      bootstrap_interval = data.frame(
        mean = bootstrap$estimate,
        standard_error = bootstrap$standard_error,
        conf_level = bootstrap$conf_level,
        lower = bootstrap$lower,
        upper = bootstrap$upper
      ),
      bootstrap = bootstrap
    ),
    class = "incrementality_analysis"
  )
}

#' @export
print.incrementality_analysis <- function(x, ...) {
  cat("Incrementality analysis\n")
  cat("Metric: ", x$metric, "\n", sep = "")
  cat("Direction: ", x$direction, "\n", sep = "")
  cat("Experiments: ", nrow(x$differences), "\n\n", sep = "")
  output <- rbind(
    data.frame(method = "Student t", x$t_interval[c(
      "mean", "standard_error", "conf_level", "lower", "upper"
    )]),
    data.frame(method = "Bootstrap", x$bootstrap_interval)
  )
  print(output, row.names = FALSE)
  invisible(x)
}
