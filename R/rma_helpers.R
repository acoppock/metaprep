#' Run rma.mv on an estimates_vcov object
#'
#' @description
#' A convenience wrapper for [metafor::rma.mv()] that automatically extracts
#' the estimates data frame and variance-covariance matrix from an estimates_vcov object.
#'
#' This function passes all arguments directly to [metafor::rma.mv()], but
#' handles the `data` and `V` arguments automatically.
#'
#' @param object An estimates_vcov object
#' @param yi Formula or bare column name specifying the effect sizes (e.g., `estimate`)
#' @param V Variance-covariance matrix (defaults to the vcov from object)
#' @param ... Additional arguments passed to [metafor::rma.mv()], such as `random`, `mods`, etc.
#'
#' @return An object of class `rma.mv` as returned by [metafor::rma.mv()]
#'
#' @examples
#' if (requireNamespace("metafor", quietly = TRUE)) {
#'   set.seed(123)
#'   dat1 <- data.frame(Y = rnorm(50), Z = sample(c("T0", "T1"), 50, TRUE))
#'   dat2 <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1", "T2"), 100, TRUE))
#'   dat3 <- data.frame(Y = rnorm(200), Z = sample(c("T0", "T1", "T2"), 200, TRUE))
#'
#'   prepped_fits <- dplyr::bind_rows(
#'     study1 = prep_fit(lm(Y ~ Z, data = dat1), term = "ZT1"),
#'     study2 = prep_fit(lm(Y ~ Z, data = dat2), term = c("ZT1", "ZT2")),
#'     study3 = prep_fit(lm(Y ~ Z, data = dat3), term = c("ZT1", "ZT2")),
#'     .id = "study"
#'   )
#'   ev <- as_estimates_vcov(prepped_fits)
#'
#'   ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#'   ev |> rma_mv_helper(yi = estimate, mods = ~ study, random = ~ 1 | id)
#'   ev |>
#'     dplyr::filter(study != "study1") |>
#'     rma_mv_helper(yi = estimate, random = ~ 1 | id)
#' }
#'
#' @importFrom rlang enexpr eval_tidy abort
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

  # Extract estimates
  estimates <- object$estimates

  # Use provided vcov if NULL
  if (is.null(V)) {
    V <- object$vcov
  }

  # Capture yi expression and evaluate in estimates context
  yi_expr <- rlang::enexpr(yi)
  yi_vec <- rlang::eval_tidy(yi_expr, data = estimates)

  # Call rma.mv with the evaluated vector
  metafor::rma.mv(
    yi = yi_vec,
    V = V,
    data = estimates,
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
#' the estimates data frame and variance estimates from an estimates_vcov object.
#'
#' Note: This function uses the diagonal of the vcov matrix as the variance
#' estimates. If you have correlated estimates (e.g., from multi-arm trials),
#' use [rma_mv_helper()] instead to properly account for the correlation structure.
#'
#' @param object An estimates_vcov object
#' @param yi Formula or bare column name specifying the effect sizes (e.g., `estimate`)
#' @param vi Numeric vector specifying the variances (defaults to diag(vcov))
#' @param ... Additional arguments passed to [metafor::rma.uni()]
#'
#' @return An object of class `rma.uni` as returned by [metafor::rma.uni()]
#'
#' @examples
#' if (requireNamespace("metafor", quietly = TRUE)) {
#'   set.seed(123)
#'   dat1 <- data.frame(Y = rnorm(50), Z = sample(c("T0", "T1"), 50, TRUE))
#'   dat2 <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1"), 100, TRUE))
#'   dat3 <- data.frame(Y = rnorm(200), Z = sample(c("T0", "T1"), 200, TRUE))
#'
#'   prepped_fits <- dplyr::bind_rows(
#'     study1 = prep_fit(lm(Y ~ Z, data = dat1), term = "ZT1"),
#'     study2 = prep_fit(lm(Y ~ Z, data = dat2), term = "ZT1"),
#'     study3 = prep_fit(lm(Y ~ Z, data = dat3), term = "ZT1"),
#'     .id = "study"
#'   )
#'   ev <- as_estimates_vcov(prepped_fits)
#'
#'   ev |> rma_uni_helper(yi = estimate)
#'   ev |>
#'     dplyr::mutate(large_study = study == "study3") |>
#'     rma_uni_helper(yi = estimate, mods = ~ large_study)
#' }
#'
#' @importFrom rlang enexpr eval_tidy abort
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

  # Extract estimates
  estimates <- object$estimates

  # Use diagonal of vcov if vi is NULL
  if (is.null(vi)) {
    vi <- diag(object$vcov)
  }

  # Capture yi expression and evaluate in estimates context
  yi_expr <- rlang::enexpr(yi)
  yi_vec <- rlang::eval_tidy(yi_expr, data = estimates)

  # Call rma.uni with the evaluated vector
  metafor::rma.uni(
    yi = yi_vec,
    vi = vi,
    data = estimates,
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
