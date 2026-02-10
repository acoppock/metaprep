#' Run rma.mv on an estimates_vcov object
#'
#' @description
#' A convenience wrapper for [metafor::rma.mv()] that automatically extracts
#' the data and variance-covariance matrix from an estimates_vcov object.
#'
#' This function passes all arguments directly to [metafor::rma.mv()], but
#' handles the data and V arguments automatically.
#'
#' @param object An estimates_vcov object
#' @param yi Formula or numeric vector specifying the effect sizes (passed to rma.mv)
#' @param V Formula or variance-covariance matrix (defaults to the vcov from object)
#' @param ... Additional arguments passed to [metafor::rma.mv()]
#'
#' @return An object of class `rma.mv` as returned by [metafor::rma.mv()]
#'
#' @examples
#' \dontrun{
#' library(metafor)
#' library(estimatr)
#' library(dplyr)
#'
#' # Create example data
#' dat <- data.frame(
#'   Y = rnorm(100),
#'   Z = factor(sample(c("T0", "T1", "T2"), 100, TRUE)),
#'   country = sample(c("USA", "UK"), 100, TRUE),
#'   cue_type = sample(c("visual", "auditory"), 100, TRUE)
#' )
#'
#' # Fit models and prep
#' prepped_fits <- dat |>
#'   nest_by(country, cue_type) |>
#'   mutate(
#'     fit = list(lm_robust(Y ~ Z, data = data)),
#'     prep_obj = list(prep_fit(fit, term = "ZT1"))
#'   ) |>
#'   unnest(prep_obj)
#'
#' # Create estimates_vcov object
#' ev <- as_estimates_vcov(prepped_fits)
#'
#' # Simple meta-analysis
#' ev |> rma_mv_helper(yi = estimate)
#'
#' # With moderators
#' ev |> rma_mv_helper(yi = estimate, mods = ~ country)
#'
#' # Filter then analyze
#' ev |>
#'   filter(country == "USA") |>
#'   rma_mv_helper(yi = estimate)
#'
#' # Group then analyze
#' ev |>
#'   nest_by(cue_type) |>
#'   mutate(
#'     rma_fit = list(rma_mv_helper(data, yi = estimate))
#'   ) |>
#'   reframe(tidy(rma_fit))
#' }
#'
#' @export
rma_mv_helper <- function(object, yi, V = NULL, ...) {
  UseMethod("rma_mv_helper")
}

#' @export
rma_mv_helper.estimates_vcov <- function(object, yi, V = NULL, ...) {
  # Check that metafor is available
  if (!requireNamespace("metafor", quietly = TRUE)) {
    rlang::abort(
      "Package 'metafor' is required for rma_mv_helper().",
      "i" = "Install it with: install.packages('metafor')"
    )
  }

  # Extract data
  data <- object$data

  # Use provided vcov if NULL
  if (is.null(V)) {
    V <- object$vcov
  }

  # Capture yi expression and evaluate in data context
  yi_expr <- rlang::enexpr(yi)
  yi_vec <- rlang::eval_tidy(yi_expr, data = data)

  # Call rma.mv with the evaluated vector
  metafor::rma.mv(
    yi = yi_vec,
    V = V,
    data = data,
    ...
  )
}

#' @export
rma_mv_helper.list <- function(object, yi, V = NULL, ...) {
  # If it's a list (from rowwise), extract first element
  if (length(object) > 0 && inherits(object[[1]], "estimates_vcov")) {
    # Pass yi as expression
    rma_mv_helper(object[[1]], yi = {{yi}}, V = V, ...)
  } else {
    rlang::abort("List does not contain an estimates_vcov object")
  }
}

#' Run rma.uni on an estimates_vcov object
#'
#' @description
#' A convenience wrapper for [metafor::rma.uni()] that automatically extracts
#' the data and variance estimates from an estimates_vcov object.
#'
#' Note: This function uses the diagonal of the vcov matrix as the variance
#' estimates. If you have correlated estimates, use [rma_mv_helper()] instead.
#'
#' @param object An estimates_vcov object
#' @param yi Formula or numeric vector specifying the effect sizes (passed to rma.uni)
#' @param vi Formula or numeric vector specifying the variances (defaults to diag(vcov))
#' @param ... Additional arguments passed to [metafor::rma.uni()]
#'
#' @return An object of class `rma.uni` as returned by [metafor::rma.uni()]
#'
#' @examples
#' \dontrun{
#' library(metafor)
#'
#' # Simple univariate meta-analysis
#' ev |> rma_uni_helper(yi = estimate)
#'
#' # With moderators
#' ev |> rma_uni_helper(yi = estimate, mods = ~ country)
#' }
#'
#' @export
rma_uni_helper <- function(object, yi, vi = NULL, ...) {
  UseMethod("rma_uni_helper")
}

#' @export
rma_uni_helper.estimates_vcov <- function(object, yi, vi = NULL, ...) {
  # Check that metafor is available
  if (!requireNamespace("metafor", quietly = TRUE)) {
    rlang::abort(
      "Package 'metafor' is required for rma_uni_helper().",
      "i" = "Install it with: install.packages('metafor')"
    )
  }

  # Extract data
  data <- object$data

  # Use diagonal of vcov if vi is NULL
  if (is.null(vi)) {
    vi <- diag(object$vcov)
  }

  # Capture yi expression and evaluate in data context
  yi_expr <- rlang::enexpr(yi)
  yi_vec <- rlang::eval_tidy(yi_expr, data = data)

  # Call rma.uni with the evaluated vector
  metafor::rma.uni(
    yi = yi_vec,
    vi = vi,
    data = data,
    ...
  )
}

#' @export
rma_uni_helper.list <- function(object, yi, vi = NULL, ...) {
  # If it's a list (from rowwise), extract first element
  if (length(object) > 0 && inherits(object[[1]], "estimates_vcov")) {
    # Pass yi as expression
    rma_uni_helper(object[[1]], yi = {{yi}}, vi = vi, ...)
  } else {
    rlang::abort("List does not contain an estimates_vcov object")
  }
}
