#' Combine variance-covariance matrices into a block-diagonal matrix
#'
#' @description
#' `got_bdiag_vcov()` takes a data frame of prepped model fits (as returned by
#' [prep_fit()]) and extracts each element of the `vcov_obj` list-column,
#' combining them into a single block-diagonal variance-covariance matrix
#' using [Matrix::bdiag()].
#'
#' This is useful when you have multiple fitted models and you want to
#' represent their combined covariance structure in a single sparse matrix.
#'
#' @param prepped_fits_df A data frame or tibble containing a list-column
#'   named `vcov_obj`, where each element is a variance-covariance matrix
#'   (typically from a fitted model).
#'
#' @return A sparse block-diagonal matrix of class `dgCMatrix` combining all
#'   variance-covariance matrices in `vcov_obj`.
#'
#' @details
#' - The function performs defensive checks to ensure the input is a data frame
#'   and that the `vcov_obj` column exists and contains only matrices.
#' - Uses [Matrix::bdiag()] internally for efficient block-diagonal binding.
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
#' # Create a block-diagonal vcov matrix
#' got_bdiag_vcov(prepped_fits_df)
#' }
#'
#' @importFrom rlang abort
#' @importFrom dplyr pull
#' @importFrom Matrix bdiag
#' @export
get_bdiag_vcov <- function(prepped_fits_df) {

  # --- Defensive checks ----
  if (!is.data.frame(prepped_fits_df)) {
    rlang::abort("`prepped_fits_df` must be a data frame or tibble.")
  }

  if (!"vcov_obj" %in% names(prepped_fits_df)) {
    rlang::abort(
      "Input must contain a list-column named `vcov_obj`.\n",
      "x" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    )
  }

  # --- Combine vcov matrices into block-diagonal form ----
  prepped_fits_df |>
    dplyr::pull(vcov_obj) |>
    Matrix::bdiag()
}
