#' Student's t confidence interval for a mean effect
#'
#' @param x Numeric vector of experiment-level differences.
#' @param conf_level Confidence level between 0 and 1.
#' @param na_rm Logical; remove missing values?
#'
#' @return A one-row data frame with the sample size, mean, standard error,
#'   confidence level, and lower and upper confidence limits.
#' @export
#'
#' @examples
#' t_confidence_interval(c(0.4, 0.8, 0.1, 0.6))
t_confidence_interval <- function(x, conf_level = 0.95, na_rm = TRUE) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  if (na_rm) x <- x[!is.na(x)]
  if (anyNA(x)) stop("`x` contains missing values.", call. = FALSE)
  if (length(x) < 2L) stop("At least two observations are required.",
                           call. = FALSE)

  estimate <- mean(x)
  standard_error <- stats::sd(x) / sqrt(length(x))
  critical_value <- stats::qt(1 - (1 - conf_level) / 2, df = length(x) - 1)
  margin <- critical_value * standard_error
  data.frame(
    n = length(x),
    mean = estimate,
    standard_error = standard_error,
    conf_level = conf_level,
    lower = estimate - margin,
    upper = estimate + margin
  )
}

#' Student's t confidence interval (legacy interface)
#'
#' @param datawithID A data frame whose second column contains differences.
#' @param confinter Legacy lower-tail alpha value. For example, `0.05` produces
#'   a 95 percent two-sided confidence interval.
#' @return A list containing the confidence interval, mean, and standard error.
#' @export
t_test_cf <- function(datawithID, confinter = 0.05) {
  if (!is.data.frame(datawithID) || ncol(datawithID) < 2L) {
    stop("`datawithID` must have at least two columns.", call. = FALSE)
  }
  if (!is.numeric(confinter) || length(confinter) != 1L ||
      is.na(confinter) || confinter <= 0 || confinter >= 1) {
    stop("`confinter` must be strictly between 0 and 1.", call. = FALSE)
  }
  result <- t_confidence_interval(datawithID[[2]], conf_level = 1 - confinter,
                                  na_rm = FALSE)
  list(
    confidence_interval = c(result$lower, result$upper),
    mean = result$mean,
    standard_error = result$standard_error
  )
}
