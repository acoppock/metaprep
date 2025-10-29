#' Extract and tidy glance-level model summaries
#'
#' @description
#' `get_glance_df()` takes a data frame of prepped model fits (as returned by
#' [prep_fit()]) and extracts the `glance_obj` list-column, unnesting it into
#' a tidy tibble with one row per fitted model.
#'
#' This is typically used to compile model-level summary statistics (such as
#' R², adjusted R², residual standard error, etc.) across multiple fitted
#' models, while dropping unneeded columns like `tidy_obj` and `vcov_obj`.
#'
#' @param prepped_fits_df A data frame or tibble containing a list-column
#'   named `glance_obj`, where each element is a single-row data frame (as
#'   returned by [broom::glance()]).
#'
#' @return A tibble containing model-level summary statistics, with one row per
#'   model.
#'
#' @details
#' - The function performs defensive checks to ensure the input is a data frame
#'   and that the `glance_obj` column exists and contains data frames.
#' - Typically used after calling [prep_fit()] and optionally combining results
#'   with [dplyr::bind_rows()].
#'
#' @examples
#' \dontrun{
#' library(estimatr)
#' library(dplyr)
#'
#' # Example data
#' dat <- data.frame(
#'   Y = rnorm(100),
#'   Z = factor(sample(c("T0", "T1", "T2"), 100, TRUE)),
#'   X = rnorm(100)
#' )
#'
#' # Fit models
#' fit_1 <- lm_robust(Y ~ Z + X, data = dat)
#' fit_2 <- lm_robust(Y ~ Z + X, data = dat)
#'
#' # Prep results
#' prepped_fit_1 <- prep_fit(fit_1, term = c("ZT1", "ZT2"))
#' prepped_fit_2 <- prep_fit(fit_2, term = c("ZT1"))
#'
#' # Combine
#' prepped_fits_df <- bind_rows(prepped_fit_1, prepped_fit_2, .id = "fit")
#'
#' # Extract glance-level summaries
#' glance_df <- get_glance_df(prepped_fits_df)
#' glance_df
#' }
#'
#' @importFrom rlang abort
#' @importFrom dplyr select any_of
#' @importFrom tidyr unnest
#' @export
get_glance_df <- function(prepped_fits_df) {

  # --- Defensive checks ----
  if (!is.data.frame(prepped_fits_df)) {
    rlang::abort("`prepped_fits_df` must be a data frame or tibble.")
  }

  if (!"glance_obj" %in% names(prepped_fits_df)) {
    rlang::abort(
      "Input must contain a list-column named `glance_obj`.\n",
      "x" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    )
  }

  # --- Extract and unnest glance-level results ----
  prepped_fits_df |>
    dplyr::select(-dplyr::any_of(c("tidy_obj", "vcov_obj"))) |>
    tidyr::unnest("glance_obj")
}
