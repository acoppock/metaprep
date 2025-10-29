#' Extract and Unnest Tidy Estimates from Prepared Model Fits
#'
#' @description
#' Takes one or more pre-processed model fits (as returned by [prep_fit()])
#' and returns a long, tidy data frame of coefficient-level estimates.
#'
#' This helper drops the model-level (`glance`) and variance-covariance (`vcov`)
#' list columns, keeping only the term-level information in a flat format suitable
#' for plotting, tabulation, or downstream analysis.
#'
#' @param prepped_fits_df A tibble or data frame created by combining one or more
#'   calls to [prep_fit()]. Must include a list-column named `tidy` (usually called
#'   `tidy_obj` or similar depending on naming).
#'
#' @return
#' A tibble containing all coefficient-level estimates with one row per term,
#' plus any identifying columns (e.g., a model ID column if multiple fits were
#' combined via [dplyr::bind_rows()]).
#'
#' @examples
#' \dontrun{
#' library(estimatr)
#' library(dplyr)
#' library(tidyr)
#'
#' # Example data
#' dat <- data.frame(
#'   Y = rnorm(100),
#'   Z = factor(sample(c("T0", "T1", "T2"), 100, TRUE)),
#'   X = rnorm(100)
#' )
#'
#' # Two model fits with slightly different term selections
#' fit_1 <- lm_robust(Y ~ Z + X, data = dat)
#' prepped_fit_1 <- prep_fit(fit_1, term = c("ZT1", "ZT2"))
#'
#' fit_2 <- lm_robust(Y ~ Z + X, data = dat)
#' prepped_fit_2 <- prep_fit(fit_2, term = c("ZT1"))
#'
#' # Combine into one tibble
#' prepped_fits_df <- dplyr::bind_rows(prepped_fit_1, prepped_fit_2, .id = "fit")
#'
#' # Extract coefficient-level data
#' estimates_df <- get_estimates_df(prepped_fits_df)
#'
#' estimates_df
#' }
#'
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @export
get_estimates_df <- function(prepped_fits_df) {

  # --- Defensive checks ----
  if (!is.data.frame(prepped_fits_df)) {
    rlang::abort("`prepped_fits_df` must be a data frame or tibble.")
  }

  # Expected list-columns
  expected_cols <- c("tidy_obj")
  has_tidy_col <- any(expected_cols %in% names(prepped_fits_df))
  if (!has_tidy_col) {
    rlang::abort(
      "Input must contain a list-column named `tidy_obj`.\n",
      "x" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    )
  }

  prepped_fits_df |>
    dplyr::select(-dplyr::any_of(c("glance", "glance_obj", "vcov", "vcov_obj"))) |>
    tidyr::unnest(cols = dplyr::any_of(c("tidy", "tidy_obj")))
}
