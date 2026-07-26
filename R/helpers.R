#' Legacy vector summary helpers
#'
#' These small helpers are retained for compatibility. They return `NA` when
#' all values are missing. `fnFreqDay()` returns the first mode when tied.
#'
#' @param x A vector.
#' @return `fnAve()`, `fnMax()`, and `fnSum()` return a numeric scalar.
#'   `fnFreqDay()` returns the name of the most frequent value, as a character
#'   scalar.
#' @name legacy_helpers
NULL

#' @rdname legacy_helpers
#' @export
fnFreqDay <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  counts <- table(x)
  names(counts)[which.max(counts)]
}

#' @rdname legacy_helpers
#' @export
fnMax <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

#' @rdname legacy_helpers
#' @export
fnAve <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

#' @rdname legacy_helpers
#' @export
fnSum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}
