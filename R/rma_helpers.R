#' Run rma.mv on an estimates_vcov object
#'
#' @description
#' A convenience wrapper for [metafor::rma.mv()] that automatically extracts
#' the estimates data frame and variance-covariance matrix from an estimates_vcov object.
#'
#' This function passes all arguments directly to [metafor::rma.mv()], but
#' handles the `data` and `V` arguments automatically.
#'
#' @details
#' Estimates must be finite. `metafor` drops non-finite rows with a warning and
#' returns a fit whose `k` is smaller than the object, which silently misaligns
#' anything joining a per-estimate quantity such as [stats::weights()] back onto
#' the estimates, so `rma_mv_helper()` errors instead and leaves the choice of
#' which estimates to drop to you. The same reasoning governs the non-finite
#' `vcov` guard applied when the object is built.
#'
#' @param object An estimates_vcov object
#' @param yi Formula or bare column name specifying the effect sizes (e.g., `estimate`)
#' @param V Variance-covariance matrix (defaults to the vcov from object)
#' @param ... Additional arguments passed to [metafor::rma.mv()], such as `random`, `mods`, etc.
#'
#' @return An object of class `rma.mv` as returned by [metafor::rma.mv()], or,
#'   when `cluster` is supplied, a `robust.rma` object from [metafor::robust()].
#'
#' @examplesIf requireNamespace("metafor", quietly = TRUE) && requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(dplyr)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
#' dat_2 <- data.frame(Z = complete_ra(100, num_arms = 3), Y = rnorm(100))
#' dat_3 <- data.frame(Z = complete_ra(200, num_arms = 4), Y = rnorm(200))
#'
#' fit_1 <- lm_robust(Y ~ Z, data = dat_1)
#' fit_2 <- lm_robust(Y ~ Z, data = dat_2)
#' fit_3 <- lm_robust(Y ~ Z, data = dat_3)
#'
#' prepped_fits <- bind_rows(
#'   study_1 = prep_fit(fit_1, term = "ZT2"),
#'   study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
#'   study_3 = prep_fit(fit_3, term = c("ZT2", "ZT3", "ZT4")),
#'   .id = "study"
#' )
#' ev <- as_estimates_vcov(prepped_fits)
#'
#' ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#' ev |> rma_mv_helper(yi = estimate, mods = ~ study, random = ~ 1 | id)
#' ev |>
#'   filter(study != "study_1") |>
#'   rma_mv_helper(yi = estimate, random = ~ 1 | id)
#'
#' # Cluster-robust (CR2) standard errors in one step (needs clubSandwich):
#' if (requireNamespace("clubSandwich", quietly = TRUE)) {
#'   ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id, cluster = study)
#' }
#'
#' @param cluster Optional clustering variable for cluster-robust (sandwich)
#'   standard errors. May be a bare column name of the estimates (e.g.
#'   `cluster = study`), a string-named column via `.data[[var]]`, or an
#'   external vector. When supplied, the fit is passed to
#'   [metafor::robust()]; when `NULL` (default) the ordinary model-based fit is
#'   returned.
#' @param clubSandwich Logical, passed to [metafor::robust()] when `cluster` is
#'   supplied. `TRUE` (default) requests CR2 cluster-robust standard errors via
#'   the clubSandwich package; `FALSE` uses metafor's CR0 estimator.
#'
#' @seealso [rma_uni_helper()] when the estimates are genuinely independent, and
#'   [estimates_vcov] for the object it reads from.
#'
#' @family meta-analysis wrappers
#' @importFrom rlang enexpr enquo eval_tidy abort
#' @export
rma_mv_helper <- function(object, yi, V = NULL, cluster = NULL,
                          clubSandwich = TRUE, ...) {
  UseMethod("rma_mv_helper")
}

#' @export
rma_mv_helper.estimates_vcov <- function(object, yi, V = NULL, cluster = NULL,
                                         clubSandwich = TRUE, ...) {
  # Check that metafor is available
  if (!requireNamespace("metafor", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'metafor' is required for rma_mv_helper().",
      "i" = "Install it with: install.packages('metafor')"
    ))
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

  # Guard: every estimate entering the pool must be finite
  check_estimates_finite(yi_vec, estimates)

  # Guard: a mods formula must reference columns on the object
  check_mods_vars(list(...)[["mods"]], estimates)

  # Call rma.mv with the evaluated vector
  fit <- metafor::rma.mv(
    yi = yi_vec,
    V = V,
    data = estimates,
    ...
  )

  # Optionally wrap in cluster-robust standard errors
  # enquo (not enexpr) captures the environment too, so `cluster` may be a bare
  # column name, a string via `.data[[var]]`, or an external vector/variable.
  cluster_vec <- rlang::eval_tidy(rlang::enquo(cluster), data = estimates)
  if (!is.null(cluster_vec)) {
    fit <- rma_robust(fit, cluster = cluster_vec, clubSandwich = clubSandwich)
  }

  fit
}

#' @export
rma_mv_helper.list <- function(object, yi, V = NULL, cluster = NULL,
                               clubSandwich = TRUE, ...) {
  # If it's a list (from rowwise), extract first element
  if (length(object) > 0 && inherits(object[[1]], "estimates_vcov")) {
    # Pass yi and cluster as expressions
    rma_mv_helper(object[[1]], yi = {{yi}}, V = V,
                  cluster = {{cluster}}, clubSandwich = clubSandwich, ...)
  } else {
    rlang::abort("List does not contain an estimates_vcov object")
  }
}

# Internal: a mods formula must reference columns that live on the
# estimates_vcov object, so moderators added with mutate() stay aligned with the
# block-diagonal vcov. Erroring here turns the silent "moderator lived only on a
# detached get_estimates_df() frame" bug into an immediate, explicit failure.
check_mods_vars <- function(mods, estimates, call = rlang::caller_env()) {
  if (is.null(mods) || !inherits(mods, "formula")) {
    return(invisible())
  }
  missing <- setdiff(all.vars(mods), names(estimates))
  if (length(missing) > 0) {
    rlang::abort(
      c(
        sprintf(
          "Moderator variable%s not found in the estimates: %s.",
          if (length(missing) > 1) "s" else "", paste(missing, collapse = ", ")
        ),
        "i" = "Add the moderator to the object with mutate() before meta-regressing, so it stays aligned with the vcov."
      ),
      call = call
    )
  }
  invisible()
}

# Internal: an estimate with no finite value must not enter a pooled fit
# silently. metafor drops such rows with a warning and returns a fit whose `k`
# is smaller than the object, so anything joining a per-estimate quantity back
# onto the estimates (`weights()`, `resid()`) silently misaligns. Erroring here
# is the mirror of the non-finite `vcov` guard in `symmetrize_vcov()`: which
# estimates to drop is the analyst's call, not the package's.
check_estimates_finite <- function(yi, estimates, arg = "yi",
                                   call = rlang::caller_env()) {
  finite <- is.finite(yi)
  if (all(finite)) {
    return(invisible())
  }
  ids <- estimates[["id"]]
  where <- if (!is.null(ids)) ids[!finite] else which(!finite)
  rlang::abort(
    c(
      sprintf("`%s` contains non-finite values and cannot enter a pooled fit.", arg),
      "i" = sprintf("%d of %d estimates are NA, NaN, or infinite.",
                    sum(!finite), length(finite)),
      "i" = sprintf("Affected id%s: %s.",
                    if (sum(!finite) > 1) "s" else "",
                    paste(where, collapse = ", ")),
      "i" = paste("metafor would drop these rows silently, so the fit would",
                  "report fewer estimates than the object holds. Filter them",
                  "out explicitly, or fix the fits that produced them.")
    ),
    call = call
  )
}

# Internal: rma.uni takes only the variances, so calling it on an object whose
# vcov has nonzero off-diagonals throws the dependence away and returns standard
# errors that are too small. The whole point of the package is to carry those
# covariances, so discarding them should be visible rather than silent.
warn_discarded_covariance <- function(vcov) {
  if (nrow(vcov) < 2) {
    return(invisible())
  }
  n_offdiag <- count_offdiag_nonzero(vcov)
  if (n_offdiag == 0) {
    return(invisible())
  }
  rlang::warn(
    c(
      sprintf(paste("Using only the diagonal of the vcov, discarding %d nonzero",
                    "covariance%s."),
              n_offdiag, if (n_offdiag > 1) "s" else ""),
      "i" = paste("These estimates are dependent, so the pooled standard error",
                  "will be too small."),
      "i" = "Use `rma_mv_helper()` to pass the full vcov to metafor::rma.mv().",
      "i" = "Pass `vi` explicitly if the univariate fit is what you want."
    ),
    class = "metaprep_discarded_covariance"
  )
}

# Internal: wrap a fitted rma object in cluster-robust SEs, guarding the
# optional clubSandwich dependency so the error is informative.
rma_robust <- function(fit, cluster, clubSandwich = TRUE) {
  if (clubSandwich && !requireNamespace("clubSandwich", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'clubSandwich' is required for clubSandwich = TRUE.",
      "i" = "Install it, or call with clubSandwich = FALSE for the CR0 estimator."
    ))
  }
  metafor::robust(fit, cluster = cluster, clubSandwich = clubSandwich)
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
#' @details
#' Taking the diagonal throws away every covariance the object carries, which
#' makes the pooled standard error too small. When `vi` is not supplied and the
#' vcov has nonzero off-diagonal entries, the function warns (with class
#' `"metaprep_discarded_covariance"`) naming how many covariances were dropped.
#' Supply `vi` explicitly when the univariate fit is genuinely what you want.
#'
#' Estimates must be finite. `metafor` drops non-finite rows with a warning and
#' returns a fit whose `k` is smaller than the object, which silently misaligns
#' anything joining a per-estimate quantity such as [stats::weights()] back onto
#' the estimates, so `rma_uni_helper()` errors instead and leaves the choice of
#' which estimates to drop to you.
#'
#' @param object An estimates_vcov object
#' @param yi Formula or bare column name specifying the effect sizes (e.g., `estimate`)
#' @param vi Numeric vector specifying the variances (defaults to diag(vcov))
#' @param ... Additional arguments passed to [metafor::rma.uni()]
#'
#' @return An object of class `rma.uni` as returned by [metafor::rma.uni()], or,
#'   when `cluster` is supplied, a `robust.rma` object from [metafor::robust()].
#'
#' @examplesIf requireNamespace("metafor", quietly = TRUE) && requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(dplyr)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
#' dat_2 <- data.frame(Z = complete_ra(100, num_arms = 2), Y = rnorm(100))
#' dat_3 <- data.frame(Z = complete_ra(200, num_arms = 2), Y = rnorm(200))
#'
#' fit_1 <- lm_robust(Y ~ Z, data = dat_1)
#' fit_2 <- lm_robust(Y ~ Z, data = dat_2)
#' fit_3 <- lm_robust(Y ~ Z, data = dat_3)
#'
#' prepped_fits <- bind_rows(
#'   study_1 = prep_fit(fit_1, term = "ZT2"),
#'   study_2 = prep_fit(fit_2, term = "ZT2"),
#'   study_3 = prep_fit(fit_3, term = "ZT2"),
#'   .id = "study"
#' )
#' ev <- as_estimates_vcov(prepped_fits)
#'
#' ev |> rma_uni_helper(yi = estimate)
#' ev |>
#'   mutate(large_study = study == "study_3") |>
#'   rma_uni_helper(yi = estimate, mods = ~ large_study)
#'
#' @param cluster Optional bare column name (evaluated in the estimates data
#'   frame, like `yi`) giving the clustering variable for cluster-robust
#'   (sandwich) standard errors via [metafor::robust()]. `NULL` (default)
#'   returns the ordinary model-based fit.
#' @param clubSandwich Logical, passed to [metafor::robust()] when `cluster` is
#'   supplied. `TRUE` (default) requests CR2 standard errors via the
#'   clubSandwich package; `FALSE` uses metafor's CR0 estimator.
#'
#' @seealso [rma_mv_helper()], which uses the full vcov and is what dependent
#'   estimates need.
#'
#' @family meta-analysis wrappers
#' @importFrom rlang enexpr enquo eval_tidy abort
#' @export
rma_uni_helper <- function(object, yi, vi = NULL, cluster = NULL,
                           clubSandwich = TRUE, ...) {
  UseMethod("rma_uni_helper")
}

#' @export
rma_uni_helper.estimates_vcov <- function(object, yi, vi = NULL, cluster = NULL,
                                          clubSandwich = TRUE, ...) {
  # Check that metafor is available
  if (!requireNamespace("metafor", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'metafor' is required for rma_uni_helper().",
      "i" = "Install it with: install.packages('metafor')"
    ))
  }

  # Extract estimates
  estimates <- object$estimates

  # Capture yi expression and evaluate in estimates context
  yi_expr <- rlang::enexpr(yi)
  yi_vec <- rlang::eval_tidy(yi_expr, data = estimates)

  # Guard: every estimate entering the pool must be finite. Checked before the
  # discarded-covariance warning so a fatal problem is not preceded by a warning
  # about a lesser one.
  check_estimates_finite(yi_vec, estimates)

  # Guard: a mods formula must reference columns on the object
  check_mods_vars(list(...)[["mods"]], estimates)

  # Use diagonal of vcov if vi is NULL. Taking the diagonal discards any
  # covariances the object carries, so say so rather than doing it quietly.
  if (is.null(vi)) {
    warn_discarded_covariance(object$vcov)
    vi <- diag(object$vcov)
  }

  # Call rma.uni with the evaluated vector
  fit <- metafor::rma.uni(
    yi = yi_vec,
    vi = vi,
    data = estimates,
    ...
  )

  # Optionally wrap in cluster-robust standard errors
  # enquo (not enexpr) captures the environment too, so `cluster` may be a bare
  # column name, a string via `.data[[var]]`, or an external vector/variable.
  cluster_vec <- rlang::eval_tidy(rlang::enquo(cluster), data = estimates)
  if (!is.null(cluster_vec)) {
    fit <- rma_robust(fit, cluster = cluster_vec, clubSandwich = clubSandwich)
  }

  fit
}

#' @export
rma_uni_helper.list <- function(object, yi, vi = NULL, cluster = NULL,
                                clubSandwich = TRUE, ...) {
  # If it's a list (from rowwise), extract first element
  if (length(object) > 0 && inherits(object[[1]], "estimates_vcov")) {
    # Pass yi and cluster as expressions
    rma_uni_helper(object[[1]], yi = {{yi}}, vi = vi,
                   cluster = {{cluster}}, clubSandwich = clubSandwich, ...)
  } else {
    rlang::abort("List does not contain an estimates_vcov object")
  }
}
