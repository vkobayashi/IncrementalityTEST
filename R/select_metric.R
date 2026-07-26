#' Pair experiment groups and calculate metric differences
#'
#' For every experiment, pairs one control value with one treatment value and
#' computes `treatment - control` by default. Each experiment must contain
#' exactly one row for each requested group.
#'
#' @param data A data frame containing experiment, group, and metric columns.
#' @param metric Character scalar naming the metric column.
#' @param id_col Character scalar naming the experiment identifier column.
#' @param group_col Character scalar naming the group column.
#' @param control Value identifying the control group.
#' @param treatment Value identifying the treatment group.
#' @param direction Difference direction: `"treatment-control"` or
#'   `"control-treatment"`.
#' @param na_action How to handle missing metric values: `"error"` or `"omit"`.
#'
#' @return A data frame with the experiment ID, control and treatment values,
#'   and a `difference` column.
#' @export
#'
#' @examples
#' results <- data.frame(
#'   experiment = rep(c("A", "B", "C"), each = 2),
#'   group = rep(c("control", "treatment"), 3),
#'   RPU = c(10, 11, 8, 9.5, 12, 11.5)
#' )
#' metric_differences(results, "RPU")
metric_differences <- function(data, metric, id_col = "experiment",
                               group_col = "group", control = "control",
                               treatment = "treatment",
                               direction = c("treatment-control",
                                             "control-treatment"),
                               na_action = c("error", "omit")) {
  direction <- match.arg(direction)
  na_action <- match.arg(na_action)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  scalar_names <- c(metric = metric, id_col = id_col, group_col = group_col)
  if (any(!vapply(scalar_names, function(x) is.character(x) && length(x) == 1L,
                  logical(1)))) {
    stop("`metric`, `id_col`, and `group_col` must be single column names.",
         call. = FALSE)
  }
  missing_cols <- setdiff(unname(scalar_names), names(data))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  if (!is.numeric(data[[metric]])) {
    stop("The metric column must be numeric.", call. = FALSE)
  }

  keep <- data[[group_col]] %in% c(control, treatment)
  selected <- data[keep, c(id_col, group_col, metric), drop = FALSE]
  if (!nrow(selected)) stop("No rows match the requested groups.", call. = FALSE)

  key <- interaction(selected[[id_col]], selected[[group_col]], drop = TRUE)
  duplicates <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(duplicates)) {
    stop("Each experiment must have exactly one row per requested group.",
         call. = FALSE)
  }

  control_data <- selected[selected[[group_col]] == control,
                           c(id_col, metric), drop = FALSE]
  treatment_data <- selected[selected[[group_col]] == treatment,
                             c(id_col, metric), drop = FALSE]
  names(control_data)[2] <- "control"
  names(treatment_data)[2] <- "treatment"
  paired <- merge(control_data, treatment_data, by = id_col, all = TRUE,
                  sort = FALSE)

  incomplete <- is.na(paired$control) | is.na(paired$treatment)
  if (any(incomplete) && na_action == "error") {
    stop("Every experiment must have non-missing control and treatment values.",
         call. = FALSE)
  }
  if (na_action == "omit") paired <- paired[!incomplete, , drop = FALSE]
  paired$difference <- if (direction == "treatment-control") {
    paired$treatment - paired$control
  } else {
    paired$control - paired$treatment
  }
  rownames(paired) <- NULL
  paired
}

#' Calculate metric differences (legacy interface)
#'
#' Compatibility wrapper for datasets containing `iabtest_id` and `abt_group`,
#' where control is coded `0` and treatment is coded `1`. It preserves the
#' original convention of control minus treatment.
#'
#' @param mydata A data frame containing `iabtest_id`, `abt_group`, and the
#'   selected metric.
#' @param metric Character scalar naming the metric.
#' @return A two-column data frame with `iabtest_id` and `inc_<metric>`.
#' @export
test_metric <- function(mydata, metric) {
  result <- metric_differences(
    data = mydata, metric = metric, id_col = "iabtest_id",
    group_col = "abt_group", control = 0, treatment = 1,
    direction = "control-treatment"
  )
  output <- result[c("iabtest_id", "difference")]
  names(output)[2] <- paste0("inc_", metric)
  output
}
