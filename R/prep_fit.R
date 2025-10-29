#' Prepare a Model Fit Object for Tidy Extraction
#'
#' @description
#' Extracts selected term-level information from a fitted model object,
#' returning a tibble with list-columns containing the tidied coefficients,
#' model summary, and corresponding variance-covariance matrix subset.
#'
#' This is particularly useful when working with multi-level treatments
#' (e.g., factor variables) and when you want to extract only a subset of terms.
#'
#' @param fit A fitted model object (e.g., from [estimatr::lm_robust()] or [stats::lm()]).
#' @param term A character vector of term names or patterns to match within the model
#'   coefficients (e.g., `"ZT1"`, `"ZT2"`).
#'
#' @return
#' A tibble with one row and the following list-columns:
#' \describe{
#'   \item{tidy}{A tibble of tidied coefficient estimates for the selected terms.}
#'   \item{glance}{A tibble of model-level summary statistics (from [broom::glance()]).}
#'   \item{vcov}{A numeric matrix of the variance-covariance subset corresponding to the selected terms.}
#' }
#'
#' @examples
#' \dontrun{
#' library(estimatr)
#' library(dplyr)
#' library(broom)
#' library(stringr)
#'
#' # Example data
#' dat <- data.frame(
#'   Y = rnorm(100),
#'   Z = factor(sample(c("T0", "T1", "T2"), 100, TRUE)),
#'   X = rnorm(100)
#' )
#'
#' # Fit robust linear model
#' fit <- lm_robust(Y ~ Z + X, data = dat)
#'
#' # Prepare only the Z-related terms
#' prepped_fit <- prep_fit(fit, term = c("ZT1", "ZT2"))
#'
#' prepped_fit
#' prepped_fit$tidy[[1]]
#' prepped_fit$vcov[[1]]
#' }
#'
#' @importFrom broom tidy glance
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @export
prep_fit <- function(fit, term) {
  stopifnot(is.character(term), length(term) > 0)

  tidy_obj <- broom::tidy(fit)
  glance_obj <- broom::glance(fit)
  vcov_obj <- vcov(fit)

  # Build regex pattern to match any of the requested terms
  pattern <- paste(term, collapse = "|")

  # Filter tidy output for matching terms
  tidy_sel <- dplyr::filter(tidy_obj, stringr::str_detect(.data$term, pattern))

  # Subset vcov to matching rows/columns
  rows <- stringr::str_detect(rownames(vcov_obj), pattern)
  cols <- stringr::str_detect(colnames(vcov_obj), pattern)
  vcov_sel <- vcov_obj[rows, cols, drop = FALSE]

  # Return a tibble with list columns
  tibble::tibble(
    tidy_obj = list(tidy_sel),
    glance_obj = list(glance_obj),
    vcov_obj = list(vcov_sel)
  )
}



