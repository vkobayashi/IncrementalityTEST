#' Calculate commerce metrics
#'
#' Calculates revenue per user (RPU), buyer rate (BR), average order value
#' (AOV), and transactions per buyer (TPB). Inputs are recycled using base R
#' rules, so the function works with individual totals or equally sized vectors.
#'
#' @param nb_transactions Numeric vector. Number of transactions.
#' @param nb_users Numeric vector. Number of unique users.
#' @param revenue Numeric vector. Revenue.
#' @param nb_buyers Numeric vector. Number of buyers.
#' @param zero_denominator How to handle division by zero: return `NA`
#'   (default) or allow R to return infinite/undefined values.
#'
#' @return A data frame with columns `RPU`, `BR`, `AOV`, and `TPB`.
#' @export
#'
#' @examples
#' calculate_metrics(
#'   nb_transactions = 11278,
#'   nb_users = 297073,
#'   revenue = 279480.4,
#'   nb_buyers = 10909
#' )
calculate_metrics <- function(nb_transactions, nb_users, revenue, nb_buyers,
                              zero_denominator = c("na", "allow")) {
  zero_denominator <- match.arg(zero_denominator)
  values <- list(
    nb_transactions = nb_transactions,
    nb_users = nb_users,
    revenue = revenue,
    nb_buyers = nb_buyers
  )
  if (any(!vapply(values, is.numeric, logical(1)))) {
    stop("All metric inputs must be numeric.", call. = FALSE)
  }
  if (any(vapply(values, function(x) any(x < 0, na.rm = TRUE), logical(1)))) {
    stop("Counts and revenue must not be negative.", call. = FALSE)
  }

  safe_divide <- function(numerator, denominator) {
    result <- numerator / denominator
    if (zero_denominator == "na") {
      result[!is.na(denominator) & denominator == 0] <- NA_real_
    }
    result
  }

  data.frame(
    RPU = safe_divide(revenue, nb_users),
    BR = safe_divide(nb_buyers, nb_users),
    AOV = safe_divide(revenue, nb_transactions),
    TPB = safe_divide(nb_transactions, nb_buyers),
    check.names = FALSE
  )
}

#' Calculate commerce metrics (legacy interface)
#'
#' This compatibility wrapper returns the same named list as the original
#' package API. New code should generally use [calculate_metrics()].
#'
#' @inheritParams calculate_metrics
#' @return A named list containing `RPU`, `BR`, `AOV`, and `TPB`.
#' @export
#'
#' @examples
#' incrementality_metrics(11278, 297073, 279480.4, 10909)
incrementality_metrics <- function(nb_transactions, nb_users, revenue,
                                   nb_buyers) {
  result <- calculate_metrics(
    nb_transactions, nb_users, revenue, nb_buyers,
    zero_denominator = "allow"
  )
  unclass(as.list(result))
}
