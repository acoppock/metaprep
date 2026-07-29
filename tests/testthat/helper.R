library(estimatr)
library(dplyr)
library(tidyr)

make_test_data <- function(n = 100) {
  set.seed(42)
  data.frame(
    Y = rnorm(n),
    Z = factor(sample(c("T0", "T1", "T2"), n, TRUE)),
    country = sample(c("USA", "UK"), n, TRUE),
    study_type = sample(c("RCT", "observational"), n, TRUE)
  )
}

# rma_uni_helper() warns when it takes the diagonal of a dependent object's
# vcov. The shared fixture below is multi-arm, so that warning is correct
# everywhere. Tests about something else (cluster =, mods, rowwise dispatch) use
# this to keep the expected warning out of the report; it muffles only that one
# class, so any other warning still surfaces. Tests about the warning itself
# assert on it directly.
quiet_uni <- function(expr) {
  withCallingHandlers(
    expr,
    metaprep_discarded_covariance = function(w) invokeRestart("muffleWarning")
  )
}

make_test_prepped_fits <- function() {
  dat <- make_test_data()

  dat |>
    dplyr::nest_by(country, study_type) |>
    dplyr::mutate(
      fit_obj = list(estimatr::lm_robust(Y ~ Z, data = data)),
      prep_obj = list(prep_fit(fit_obj, term = c("ZT1", "ZT2")))
    ) |>
    tidyr::unnest(prep_obj) |>
    dplyr::ungroup()
}
