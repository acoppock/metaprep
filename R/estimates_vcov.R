#' Create an estimates_vcov object
#'
#' @description
#' Combines tidy coefficient estimates with their corresponding variance-covariance
#' matrix into a single object that maintains synchronization through dplyr operations.
#'
#' This is particularly useful for meta-analysis workflows where you need to filter,
#' group, or manipulate estimates while keeping the vcov matrix in sync.
#'
#' @param prepped_fits_df A tibble created by combining one or more calls to
#'   [prep_fit()]. Must include list-columns `tidy_obj` and `vcov_obj`.
#'
#' @return An object of class `estimates_vcov` containing:
#' \describe{
#'   \item{estimates}{A tibble of unnested coefficient estimates with an `id` column}
#'   \item{vcov}{A block-diagonal variance-covariance matrix with rownames/colnames matching `id`}
#'   \item{row_map}{Integer vector tracking original row indices}
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
#' prepped_fits <- bind_rows(
#'   study_1 = prep_fit(fit_1, term = "ZT2"),
#'   study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
#'   study_3 = prep_fit(fit_3, term = c("ZT2", "ZT3", "ZT4")),
#'   .id = "study"
#' )
#' ev <- as_estimates_vcov(prepped_fits)
#' ev
#'
#' # dplyr verbs keep the vcov synchronized
#' ev |> filter(study == "study_2")
#' ev |> arrange(estimate)
#'
#' # Pass straight to metafor via the helper
#' ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#'
#' @importFrom dplyr select pull
#' @importFrom tidyr unnest
#' @importFrom Matrix bdiag
#' @importFrom tibble as_tibble add_column
#' @importFrom rlang abort
#' @export
as_estimates_vcov <- function(prepped_fits_df) {
  # --- Defensive checks ----
  if (!is.data.frame(prepped_fits_df)) {
    rlang::abort("`prepped_fits_df` must be a data frame or tibble.")
  }

  if (!all(c("tidy_obj", "vcov_obj") %in% names(prepped_fits_df))) {
    rlang::abort(
      "Input must contain list-columns named `tidy_obj` and `vcov_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    )
  }

  # Extract estimates and vcov using the unified methods
  estimates <- get_estimates_df(prepped_fits_df)
  vcov <- get_vcov(prepped_fits_df) |> as.matrix()

  # Validate dimensions
  if (nrow(estimates) != nrow(vcov)) {
    rlang::abort(
      "Dimension mismatch between estimates and vcov matrix.",
      "i" = sprintf("estimates has %d rows, vcov has %d rows", nrow(estimates), nrow(vcov))
    )
  }

  new_estimates_vcov(estimates, vcov)
}

#' Create estimates_vcov from separate estimates and vcov
#'
#' @description
#' Alternative constructor for estimates_vcov objects when you already have
#' the estimates data frame and block-diagonal vcov matrix separately.
#'
#' This is useful if you've already called [get_estimates_df()] and
#' [get_vcov()] and want to combine them into a synchronized object.
#'
#' @param estimates_df A data frame or tibble of coefficient estimates,
#'   typically from [get_estimates_df()].
#' @param vcov_matrix A variance-covariance matrix, typically from
#'   [get_vcov()]. Must have dimensions matching the number of
#'   rows in estimates_df.
#'
#' @return An object of class `estimates_vcov`
#'
#' @examplesIf requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat <- data.frame(Z = complete_ra(100, num_arms = 2), Y = rnorm(100))
#' fit <- lm_robust(Y ~ Z, data = dat)
#' prepped <- prep_fit(fit, term = "ZT2")
#' estimates_df <- get_estimates_df(prepped)
#' vcov_matrix <- as.matrix(get_vcov(prepped))
#' estimates_vcov_from_pieces(estimates_df, vcov_matrix)
#'
#' @export
estimates_vcov_from_pieces <- function(estimates_df, vcov_matrix) {
  # --- Defensive checks ----
  if (!is.data.frame(estimates_df)) {
    rlang::abort("`estimates_df` must be a data frame or tibble.")
  }

  if (!is.matrix(vcov_matrix)) {
    # Try to coerce from sparse matrix
    if (inherits(vcov_matrix, "Matrix")) {
      vcov_matrix <- as.matrix(vcov_matrix)
    } else {
      rlang::abort("`vcov_matrix` must be a matrix or Matrix object.")
    }
  }

  # Validate dimensions
  if (nrow(estimates_df) != nrow(vcov_matrix)) {
    rlang::abort(
      "Dimension mismatch between estimates and vcov matrix.",
      "i" = sprintf(
        "estimates_df has %d rows, vcov_matrix has %d rows",
        nrow(estimates_df), nrow(vcov_matrix)
      )
    )
  }

  if (nrow(vcov_matrix) != ncol(vcov_matrix)) {
    rlang::abort(
      "vcov_matrix must be square.",
      "i" = sprintf(
        "vcov_matrix has dimensions %d x %d",
        nrow(vcov_matrix), ncol(vcov_matrix)
      )
    )
  }

  # Create the object
  new_estimates_vcov(estimates_df, vcov_matrix)
}

#' Low-level constructor for estimates_vcov
#'
#' @keywords internal
new_estimates_vcov <- function(estimates, vcov, row_map = NULL) {
  # Validate
  stopifnot(
    is.data.frame(estimates),
    is.matrix(vcov),
    nrow(estimates) == nrow(vcov),
    nrow(vcov) == ncol(vcov)
  )

  if (is.null(row_map)) {
    row_map <- seq_len(nrow(estimates))
  }

  # Add id column if not present, and set rownames/colnames on vcov
  estimates <- tibble::as_tibble(estimates)
  if (!"id" %in% names(estimates)) {
    estimates <- tibble::add_column(estimates, id = as.character(row_map), .before = 1)
  }

  # Set rownames and colnames on vcov matrix to match id
  rownames(vcov) <- estimates$id
  colnames(vcov) <- estimates$id

  structure(
    list(
      estimates = estimates,
      vcov = vcov,
      row_map = row_map
    ),
    class = "estimates_vcov"
  )
}

#' @export
print.estimates_vcov <- function(x, ...) {
  cat("<estimates_vcov>\n")
  cat(sprintf("# %d estimates with %dx%d vcov matrix\n",
              nrow(x$estimates), nrow(x$vcov), ncol(x$vcov)))
  cat("\n")
  print(x$estimates, ...)
  invisible(x)
}

#' @export
as.data.frame.estimates_vcov <- function(x, ...) {
  x$estimates
}

#' @export
as_tibble.estimates_vcov <- function(x, ...) {
  x$estimates
}

# ---- Accessor functions ----

#' Extract estimates from prepped fits or estimates_vcov object
#'
#' @description
#' Generic function to extract estimates. Works with both:
#' - `prepped_fits` tibbles (unnests tidy_obj)
#' - `estimates_vcov` objects (extracts $estimates)
#'
#' @param x Either a prepped_fits tibble or an estimates_vcov object
#' @param ... Additional arguments passed to methods
#' 
#' @return A tibble of coefficient estimates
#' 
#' @examplesIf requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat <- data.frame(Z = complete_ra(100, num_arms = 2), Y = rnorm(100))
#' fit <- lm_robust(Y ~ Z, data = dat)
#' prepped <- prep_fit(fit, term = "ZT2")
#' get_estimates_df(prepped)
#'
#' ev <- as_estimates_vcov(prepped)
#' get_estimates_df(ev)
#'
#' @importFrom dplyr select any_of
#' @importFrom tidyr unnest
#' @importFrom rlang abort
#' @export
get_estimates_df <- function(x, ...) {
  UseMethod("get_estimates_df")
}

#' @export
get_estimates_df.default <- function(x, ...) {
  rlang::abort("`x` must be a data frame or tibble.")
}

#' @export
get_estimates_df.data.frame <- function(x, ...) {
  expected_cols <- c("tidy_obj")
  has_tidy_col <- any(expected_cols %in% names(x))
  if (!has_tidy_col) {
    rlang::abort(
      "Input must contain a list-column named `tidy_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    )
  }

  x |>
    dplyr::select(-dplyr::any_of(c("glance", "glance_obj", "vcov", "vcov_obj"))) |>
    tidyr::unnest(cols = dplyr::any_of(c("tidy", "tidy_obj")))
}

#' @export
get_estimates_df.estimates_vcov <- function(x, ...) {
  x$estimates
}

#' Extract glance summary from prepped fits or estimates_vcov object
#'
#' @description
#' Generic function to extract model-level summaries. Works with:
#' - `prepped_fits` tibbles (unnests glance_obj)
#' - `estimates_vcov` objects (not applicable - returns NULL with warning)
#'
#' @param x Either a prepped_fits tibble or an estimates_vcov object
#' @param ... Additional arguments passed to methods
#' 
#' @return A tibble of model-level statistics (or NULL for estimates_vcov)
#' 
#' @examplesIf requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat <- data.frame(Z = complete_ra(100, num_arms = 2), Y = rnorm(100))
#' fit <- lm_robust(Y ~ Z, data = dat)
#' prepped <- prep_fit(fit, term = "ZT2")
#' get_glance_df(prepped)
#'
#' @importFrom dplyr select any_of
#' @importFrom tidyr unnest
#' @importFrom rlang abort warn
#' @export
get_glance_df <- function(x, ...) {
  UseMethod("get_glance_df")
}

#' @export
get_glance_df.default <- function(x, ...) {
  rlang::abort("`x` must be a data frame or tibble.")
}

#' @export
get_glance_df.data.frame <- function(x, ...) {
  if (!"glance_obj" %in% names(x)) {
    rlang::abort(
      "Input must contain a list-column named `glance_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    )
  }

  x |>
    dplyr::select(-dplyr::any_of(c("tidy_obj", "vcov_obj"))) |>
    tidyr::unnest("glance_obj")
}

#' @export
get_glance_df.estimates_vcov <- function(x, ...) {
  rlang::warn(
    "glance information is not stored in estimates_vcov objects.",
    "i" = "Extract glance from prepped_fits before creating estimates_vcov."
  )
  NULL
}

#' Extract variance-covariance matrix from prepped fits or estimates_vcov object
#'
#' @description
#' Generic function to extract vcov matrix. Works with both:
#' - `prepped_fits` tibbles (creates block-diagonal matrix)
#' - `estimates_vcov` objects (extracts $vcov)
#'
#' @param x Either a prepped_fits tibble or an estimates_vcov object
#' @param ... Additional arguments passed to methods
#' 
#' @return A variance-covariance matrix (block-diagonal for prepped_fits)
#' 
#' @examplesIf requireNamespace("metafor", quietly = TRUE) && requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(dplyr)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
#' dat_2 <- data.frame(Z = complete_ra(100, num_arms = 3), Y = rnorm(100))
#'
#' fit_1 <- lm_robust(Y ~ Z, data = dat_1)
#' fit_2 <- lm_robust(Y ~ Z, data = dat_2)
#'
#' prepped_fits <- bind_rows(
#'   study_1 = prep_fit(fit_1, term = "ZT2"),
#'   study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
#'   .id = "study"
#' )
#' # Block-diagonal vcov across studies, ready for metafor
#' get_vcov(prepped_fits)
#'
#' ev <- as_estimates_vcov(prepped_fits)
#' ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#'
#' @importFrom dplyr pull
#' @importFrom Matrix bdiag
#' @importFrom rlang abort
#' @export
get_vcov <- function(x, ...) {
  UseMethod("get_vcov")
}

#' @export
get_vcov.default <- function(x, ...) {
  rlang::abort("`x` must be a data frame or tibble.")
}

#' @export
get_vcov.data.frame <- function(x, ...) {
  if (!"vcov_obj" %in% names(x)) {
    rlang::abort(
      "Input must contain a list-column named `vcov_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    )
  }

  x |>
    dplyr::pull("vcov_obj") |>
    Matrix::bdiag()
}

#' @export
get_vcov.estimates_vcov <- function(x, ...) {
  x$vcov
}
