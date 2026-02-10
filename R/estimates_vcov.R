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
#'   \item{data}{A tibble of unnested coefficient estimates}
#'   \item{vcov}{A block-diagonal variance-covariance matrix}
#'   \item{row_map}{Integer vector tracking original row indices}
#' }
#'
#' @examples
#' \dontrun{
#' library(estimatr)
#' library(dplyr)
#'
#' # Fit models across groups
#' prepped_fits <- dat |>
#'   nest_by(study, country) |>
#'   mutate(
#'     fit = list(lm_robust(Y ~ Z, data = data)),
#'     prep = list(prep_fit(fit, term = c("ZT1", "ZT2")))
#'   ) |>
#'   unnest(prep)
#'
#' # Create estimates_vcov object
#' ev <- as_estimates_vcov(prepped_fits)
#'
#' # Now use with dplyr verbs
#' ev |> filter(country == "USA")
#' ev |> arrange(estimate)
#' ev |> mutate(abs_estimate = abs(estimate))
#'
#' # And with meta-analysis
#' ev |>
#'   filter(country == "USA") |>
#'   rma_mv_helper(yi = estimate)
#' }
#'
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

  # Extract tidy data (unnested)
  data <- prepped_fits_df |>
    dplyr::select(-dplyr::any_of(c("glance_obj", "vcov_obj"))) |>
    tidyr::unnest(cols = "tidy_obj")

  # Extract vcov matrix (block diagonal)
  vcov <- prepped_fits_df |>
    dplyr::pull(vcov_obj) |>
    Matrix::bdiag() |>
    as.matrix()

  # Validate dimensions
  if (nrow(data) != nrow(vcov)) {
    rlang::abort(
      "Dimension mismatch between estimates and vcov matrix.",
      "i" = sprintf("data has %d rows, vcov has %d rows", nrow(data), nrow(vcov))
    )
  }

  new_estimates_vcov(data, vcov)
}

#' Low-level constructor for estimates_vcov
#'
#' @keywords internal
new_estimates_vcov <- function(data, vcov, row_map = NULL) {
  # Validate
  stopifnot(
    is.data.frame(data),
    is.matrix(vcov),
    nrow(data) == nrow(vcov),
    nrow(vcov) == ncol(vcov)
  )

  if (is.null(row_map)) {
    row_map <- seq_len(nrow(data))
  }

  structure(
    list(
      data = tibble::as_tibble(data),
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
              nrow(x$data), nrow(x$vcov), ncol(x$vcov)))
  cat("\n")
  print(x$data, ...)
  invisible(x)
}

#' @export
as.data.frame.estimates_vcov <- function(x, ...) {
  x$data
}

#' @export
as_tibble.estimates_vcov <- function(x, ...) {
  x$data
}

# ---- Accessor functions ----

#' Extract the data component
#' @param x An estimates_vcov object
#' @export
get_data <- function(x) {
  UseMethod("get_data")
}

#' @export
get_data.estimates_vcov <- function(x) {
  x$data
}

#' Extract the vcov matrix
#' @param x An estimates_vcov object
#' @export
get_vcov <- function(x) {
  UseMethod("get_vcov")
}

#' @export
get_vcov.estimates_vcov <- function(x) {
  x$vcov
}

# ---- Vcov fixing helper ----

#' Fix common numerical issues in variance-covariance matrices
#'
#' @description
#' This helper function addresses common numerical issues that can occur in
#' variance-covariance matrices, particularly floating-point errors that result
#' in matrices that are not perfectly symmetric or positive semi-definite.
#'
#' @param vcov A variance-covariance matrix (or estimates_vcov object)
#' @param method Method to use for fixing. Options:
#'   - "symmetrize": Forces symmetry by averaging with transpose
#'   - "near_psd": Projects to nearest positive semi-definite matrix
#'   - "both": Applies both fixes (default)
#' @param tol Tolerance for determining if a matrix is symmetric or PSD
#'
#' @return A fixed variance-covariance matrix (or estimates_vcov object with fixed vcov)
#'
#' @details
#' Common issues this fixes:
#' - Floating-point asymmetry: When vcov[i,j] ≠ vcov[j,i] due to numerical precision
#' - Negative eigenvalues: Small negative eigenvalues due to numerical error
#'
#' The "near_psd" method uses eigenvalue decomposition and sets negative eigenvalues to zero.
#'
#' @examples
#' \dontrun{
#' # Fix a vcov matrix directly
#' fixed_vcov <- fix_vcov(my_vcov_matrix)
#'
#' # Fix an estimates_vcov object
#' ev_fixed <- fix_vcov(ev)
#'
#' # Just symmetrize
#' fixed_vcov <- fix_vcov(my_vcov_matrix, method = "symmetrize")
#' }
#'
#' @export
fix_vcov <- function(vcov, method = c("both", "symmetrize", "near_psd"), tol = 1e-10) {
  UseMethod("fix_vcov")
}

#' @export
fix_vcov.matrix <- function(vcov, method = c("both", "symmetrize", "near_psd"), tol = 1e-10) {
  method <- match.arg(method)
  
  if (method %in% c("symmetrize", "both")) {
    # Force symmetry by averaging with transpose
    vcov <- (vcov + t(vcov)) / 2
  }
  
  if (method %in% c("near_psd", "both")) {
    # Project to nearest positive semi-definite matrix
    eig <- eigen(vcov, symmetric = TRUE)
    
    # Set negative eigenvalues to zero
    eig$values[eig$values < tol] <- 0
    
    # Reconstruct matrix
    vcov <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  }
  
  vcov
}

#' @export
fix_vcov.estimates_vcov <- function(vcov, method = c("both", "symmetrize", "near_psd"), tol = 1e-10) {
  method <- match.arg(method)
  
  # Fix the vcov matrix
  fixed_vcov <- fix_vcov(vcov$vcov, method = method, tol = tol)
  
  # Return new estimates_vcov object with fixed vcov
  new_estimates_vcov(vcov$data, fixed_vcov, vcov$row_map)
}
