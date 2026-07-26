#' Bootstrap confidence interval for a mean effect
#'
#' Uses ordinary nonparametric resampling of experiment-level differences.
#'
#' @param x Numeric vector of differences.
#' @param times Number of bootstrap replicates.
#' @param conf_level Confidence level between 0 and 1.
#' @param type Interval type. Currently percentile intervals are supported.
#' @param seed Optional integer seed. The caller's random-number state is
#'   restored after the function returns.
#' @param na_rm Logical; remove missing values?
#'
#' @return An object of class `incrementality_bootstrap`, represented as a list
#'   with the estimate, standard error, interval, and bootstrap replicates.
#' @export
#'
#' @examples
#' bootstrap_incrementality(c(0.4, 0.8, 0.1, 0.6), times = 199, seed = 1)
bootstrap_incrementality <- function(x, times = 2000L, conf_level = 0.95,
                                     type = "percentile", seed = NULL,
                                     na_rm = TRUE) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  if (na_rm) x <- x[!is.na(x)]
  if (anyNA(x)) stop("`x` contains missing values.", call. = FALSE)
  if (length(x) < 2L) stop("At least two observations are required.",
                           call. = FALSE)
  if (!is.numeric(times) || length(times) != 1L || is.na(times) ||
      times < 2 || times != as.integer(times)) {
    stop("`times` must be an integer of at least 2.", call. = FALSE)
  }
  times <- as.integer(times)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1.", call. = FALSE)
  }
  if (!identical(type, "percentile")) {
    stop("Only `type = \"percentile\"` is currently supported.",
         call. = FALSE)
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      stop("`seed` must be a single number or NULL.", call. = FALSE)
    }
    existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (existed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (existed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  replicates <- replicate(times, mean(sample(x, length(x), replace = TRUE)))
  alpha <- 1 - conf_level
  limits <- unname(stats::quantile(
    replicates, probs = c(alpha / 2, 1 - alpha / 2), names = FALSE,
    type = 6
  ))
  structure(
    list(
      estimate = mean(x),
      standard_error = stats::sd(replicates),
      conf_level = conf_level,
      lower = limits[1],
      upper = limits[2],
      times = times,
      replicates = as.numeric(replicates)
    ),
    class = "incrementality_bootstrap"
  )
}

#' Statistic for the bootstrap (legacy interface)
#'
#' @param datadiff Numeric vector of differences.
#' @param indices Resampled indices.
#' @return Mean and estimated variance of the mean.
#' @export
incrementality_func <- function(datadiff, indices) {
  d <- datadiff[indices]
  c(mean(d), stats::var(d) / length(d))
}

#' Run a bootstrap (legacy interface)
#'
#' Requires the suggested `boot` package and returns a `boot` object.
#'
#' @param mydatadiff Numeric vector of differences.
#' @param statisticfunc Statistic function accepted by [boot::boot()].
#' @param nb_boot Number of bootstrap samples.
#' @return An object returned by [boot::boot()].
#' @export
res_boot <- function(mydatadiff, statisticfunc = incrementality_func,
                     nb_boot = 2000L) {
  if (!requireNamespace("boot", quietly = TRUE)) {
    stop("Install the `boot` package to use `res_boot()`.", call. = FALSE)
  }
  boot::boot(mydatadiff, statistic = statisticfunc, R = nb_boot)
}
