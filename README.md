# IncrementalityTEST

`IncrementalityTEST` is an R package for analyzing collections of randomized
incrementality experiments. It pairs treatment and control observations,
calculates experiment-level effects, and estimates uncertainty using Student's
t and nonparametric bootstrap confidence intervals.

## Installation

```r
# install.packages("remotes")
remotes::install_github("vkobayashi/IncrementalityTEST")
```

## Quick start

```r
library(IncrementalityTEST)

results <- data.frame(
  experiment = rep(paste0("test_", 1:5), each = 2),
  group = rep(c("control", "treatment"), 5),
  RPU = c(10, 11.2, 8, 8.7, 12, 13.1, 9, 9.8, 11, 12.4)
)

analysis <- analyze_incrementality(
  results,
  metric = "RPU",
  bootstrap_times = 2000,
  seed = 2026
)

analysis
analysis$differences
```

## Main functions

- `calculate_metrics()` calculates RPU, BR, AOV, and TPB safely.
- `metric_differences()` validates and pairs treatment/control observations.
- `t_confidence_interval()` estimates a confidence interval for a mean effect.
- `bootstrap_incrementality()` produces a reproducible percentile interval.
- `analyze_incrementality()` runs the complete workflow.

The original `incrementality_metrics()`, `test_metric()`, `t_test_cf()`,
`incrementality_func()`, and `res_boot()` interfaces remain available for
existing code.

## Data assumptions

Each row represents one group-level result within one experiment. An experiment
must have exactly one control row and one treatment row. By default the effect
is:

```text
treatment metric - control metric
```

Positive effects therefore indicate improvement under treatment. This package
summarizes a collection of experiment-level effects; it does not replace
user-level randomization checks or experiment-specific power analysis.

See `vignette("incrementality-workflow")` for a complete tutorial.

## Development

```r
install.packages(c("devtools", "testthat", "knitr", "rmarkdown"))
devtools::document()
devtools::test()
devtools::check()
```

Issues and contributions are welcome through the
[GitHub repository](https://github.com/vkobayashi/IncrementalityTEST).
