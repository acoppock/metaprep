#' Prepare a Model Fit Object for Tidy Extraction
#'
#' @description
#' Extracts selected term-level information from a fitted model object,
#' returning a tibble with list-columns containing the tidied coefficients,
#' model summary, and corresponding variance-covariance matrix subset.
#'
#' This function works with any model that has `tidy()`, `glance()`, and 
#' `vcov()` methods defined. For multivariate models (multiple outcomes), 
#' the function will attempt to construct appropriate term names by combining
#' outcome and term names.
#'
#' @param fit A fitted model object with `tidy()`, `glance()`, and `vcov()` methods.
#' @param term Terms to keep from the model. Either a character vector of exact
#'   term names (or regex patterns when `match = "regex"`), or a tidyselect
#'   expression resolved against the model's term names, e.g. `starts_with("Z")`,
#'   `matches("^Z_treated$")`, or `starts_with("Z_treated") & !contains(":")` to
#'   take a treatment's main effect while dropping its interaction terms.
#' @param match How to match `term` against coefficient names. `"exact"` (default)
#'   requires the term to match a coefficient name exactly. `"regex"` uses each element
#'   of `term` as a regular expression (the elements are collapsed with `|`).
#' @param handle_multivariate Logical. If `TRUE` (default), attempts to detect and
#'   handle multivariate models by creating term names in the format "outcome:term".
#'   Set to `FALSE` if you want to use the term names as-is from `tidy()`.
#'
#' @return
#' A tibble with one row and the following list-columns:
#' \describe{
#'   \item{tidy_obj}{A tibble of tidied coefficient estimates for the selected terms.}
#'   \item{glance_obj}{A tibble of model-level summary statistics (from [broom::glance()]).}
#'   \item{vcov_obj}{A numeric matrix of the variance-covariance subset corresponding to the selected terms.}
#' }
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
#' # Extract the treatment arms from a fit
#' prep_fit(fit_1, term = "ZT2")
#'
#' # Regex matching captures all ZT-prefixed terms at once
#' prep_fit(fit_3, term = "ZT", match = "regex")
#'
#' # Or select terms with tidyselect (no hand-built coefficient-name vector)
#' prep_fit(fit_3, starts_with("Z"))
#'
#' # Combine studies, build an estimates_vcov object, and meta-analyze
#' prepped_fits <- bind_rows(
#'   study_1 = prep_fit(fit_1, term = "ZT2"),
#'   study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
#'   study_3 = prep_fit(fit_3, term = c("ZT2", "ZT3", "ZT4")),
#'   .id = "study"
#' )
#' ev <- as_estimates_vcov(prepped_fits)
#' ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#'
#' @importFrom broom tidy glance
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom rlang abort warn enquo eval_tidy set_names .data .env
#' @importFrom tidyselect eval_select
#' @importFrom stats vcov
#' @export
prep_fit <- function(fit, term, match = c("exact", "regex"), handle_multivariate = TRUE) {
  match <- match.arg(match)
  stopifnot(
    is.logical(handle_multivariate),
    length(handle_multivariate) == 1
  )

  # `term` may be a character vector (exact names, or regex when match = "regex")
  # or a tidyselect expression (starts_with(), matches(), &/!/-, ...) resolved
  # against the model's term names.
  term_quo <- rlang::enquo(term)
  ts_expr <- structure(list(), class = "metaprep_tidyselect_expr")
  term_val <- tryCatch(rlang::eval_tidy(term_quo), error = function(e) ts_expr)
  use_tidyselect <- identical(term_val, ts_expr)
  if (!use_tidyselect) {
    if (!is.character(term_val) || length(term_val) == 0) {
      rlang::abort(
        "`term` must be a non-empty character vector or a tidyselect expression (e.g. starts_with(\"Z\"))."
      )
    }
    term <- term_val
  }

  # Try to get tidy, glance, and vcov - these should work for most models
  tidy_obj <- tryCatch(
    tibble::as_tibble(broom::tidy(fit)),
    error = function(e) {
      rlang::abort(
        c(
          "Could not extract tidy coefficients from model object.",
          "i" = "Make sure the model has a tidy() method available."
        ),
        parent = e
      )
    }
  )
  
  glance_obj <- tryCatch(
    broom::glance(fit),
    error = function(e) {
      rlang::warn(
        c(
          "Could not extract glance summary from model object. Using NA values.",
          "i" = "Make sure the model has a glance() method available."
        )
      )
      # Return a minimal glance object with NAs
      data.frame(
        nobs = NA_real_,
        logLik = NA_real_,
        AIC = NA_real_,
        BIC = NA_real_
      )
    }
  )
  
  vcov_obj <- tryCatch(
    stats::vcov(fit),
    error = function(e) {
      rlang::abort(
        c(
          "Could not extract vcov matrix from model object.",
          "i" = "Make sure the model has a vcov() method available."
        ),
        parent = e
      )
    }
  )

  # Handle multivariate models if requested
  if (handle_multivariate) {
    # Check if this looks like a multivariate model
    # For lm_robust: fit$outcome and fit$term exist
    # For other models: check if tidy_obj has outcome column or response column
    is_multivariate <- FALSE
    
    # Method 1: lm_robust style (has outcome and term components)
    if (!is.null(fit$outcome) && !is.null(fit$term)) {
      n_y <- length(fit$outcome)
      p <- length(fit$term)
      
      if (n_y > 1) {
        is_multivariate <- TRUE
        tidy_obj <- tidy_obj |>
          dplyr::mutate(term = paste0(
            rep(fit$outcome, each = p),
            ":",
            rep(fit$term, times = n_y)
          ))
        
        # Use NA glance for multivariate models
        glance_obj <- data.frame(
          r.squared = NA_real_,
          adj.r.squared = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          df.residual = NA_real_,
          nobs = NA_real_
        )
      }
    }
    
    # Method 2: Check for outcome/response column in tidy_obj
    if (!is_multivariate && "outcome" %in% names(tidy_obj)) {
      outcomes <- unique(tidy_obj$outcome)
      if (length(outcomes) > 1) {
        is_multivariate <- TRUE
        tidy_obj <- tidy_obj |>
          dplyr::mutate(term = paste0(.data$outcome, ":", .data$term))
      }
    }

    if (!is_multivariate && "response" %in% names(tidy_obj)) {
      responses <- unique(tidy_obj$response)
      if (length(responses) > 1) {
        is_multivariate <- TRUE
        tidy_obj <- tidy_obj |>
          dplyr::mutate(term = paste0(.data$response, ":", .data$term))
      }
    }
  }

  # Filter tidy output and subset vcov for matching terms
  if (use_tidyselect) {
    selected <- names(tidyselect::eval_select(term_quo, rlang::set_names(tidy_obj$term)))
    tidy_sel <- dplyr::filter(tidy_obj, .data$term %in% selected)
    rows <- rownames(vcov_obj) %in% selected
    cols <- colnames(vcov_obj) %in% selected
  } else if (match == "exact") {
    tidy_sel <- dplyr::filter(tidy_obj, .data$term %in% .env$term)
    rows <- rownames(vcov_obj) %in% term
    cols <- colnames(vcov_obj) %in% term
  } else {
    pattern <- paste(term, collapse = "|")
    tidy_sel <- dplyr::filter(tidy_obj, stringr::str_detect(.data$term, pattern))
    rows <- stringr::str_detect(rownames(vcov_obj), pattern)
    cols <- stringr::str_detect(colnames(vcov_obj), pattern)
  }
  vcov_sel <- vcov_obj[rows, cols, drop = FALSE]

  # Return a tibble with list columns
  tibble::tibble(
    tidy_obj = list(tidy_sel),
    glance_obj = list(glance_obj),
    vcov_obj = list(vcov_sel)
  )
}
