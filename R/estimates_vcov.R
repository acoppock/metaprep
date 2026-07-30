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
#'   \item{row_map}{Internal bookkeeping; see [estimates_vcov] for what is and is
#'     not part of the public interface}
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
#' @seealso [make_estimates_vcov()] to build the object from an estimates data
#'   frame and a vcov matrix you already have (e.g. a bootstrapped covariance
#'   across experiments that share subjects), and [estimates_vcov] for what the
#'   resulting object guarantees.
#'
#' @family estimates_vcov objects
#' @importFrom dplyr select pull
#' @importFrom tidyr unnest
#' @importFrom Matrix bdiag
#' @importFrom tibble as_tibble add_column
#' @importFrom rlang abort
#' @importFrom methods is
#' @export
as_estimates_vcov <- function(prepped_fits_df) {
  # --- Defensive checks ----
  if (!is.data.frame(prepped_fits_df)) {
    rlang::abort("`prepped_fits_df` must be a data frame or tibble.")
  }

  if (!all(c("tidy_obj", "vcov_obj") %in% names(prepped_fits_df))) {
    rlang::abort(c(
      "Input must contain list-columns named `tidy_obj` and `vcov_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    ))
  }

  # Extract estimates and vcov using the unified methods
  estimates <- get_estimates_df(prepped_fits_df)
  vcov <- get_vcov(prepped_fits_df)

  # Validate dimensions
  if (nrow(estimates) != nrow(vcov)) {
    rlang::abort(c(
      "Dimension mismatch between estimates and vcov matrix.",
      "i" = sprintf("estimates has %d rows, vcov has %d rows", nrow(estimates), nrow(vcov))
    ))
  }

  # Repair floating-point asymmetry; error on anything larger (see symmetrize_vcov)
  vcov <- symmetrize_vcov(vcov)

  new_estimates_vcov(estimates, vcov)
}

#' Create an estimates_vcov from estimates and a vcov you already have
#'
#' @description
#' Constructor for `estimates_vcov` objects from an estimates data frame and a
#' variance-covariance matrix supplied directly, rather than read off fitted
#' models by [as_estimates_vcov()].
#'
#' Use it whenever the covariances do not come out of a single regression. The
#' main case is estimates that are correlated because they share subjects but
#' cannot be stacked into one model: several experiments run on overlapping
#' samples, where the covariance between their estimates is obtained by
#' bootstrapping the whole design and taking `cov()` of the replicate estimates.
#' It also serves the plumbing case of recombining the output of
#' [get_estimates_df()] and [get_vcov()].
#'
#' The vcov is matched to the estimates **by position**: row `i` of
#' `estimates_df` is row and column `i` of `vcov_matrix`. Any dimnames on
#' `vcov_matrix` are discarded and replaced with the object's `id`, so build
#' both from the same ordered set of terms.
#'
#' @param estimates_df A data frame or tibble of coefficient estimates. Must
#'   contain an `estimate` column to be usable downstream; a `std.error` column
#'   (typically `sqrt(diag(vcov_matrix))`) is recommended so that
#'   [rescale_estimates_vcov()] has standard errors to rescale.
#' @param vcov_matrix A variance-covariance matrix, in the same row order as
#'   `estimates_df`. Must be square, symmetric, and of the same dimension as
#'   `nrow(estimates_df)`. A base matrix or a `Matrix` object are both accepted
#'   and neither is converted, so the storage of the result follows what you
#'   supply.
#'
#' @return An object of class `estimates_vcov`
#'
#' @seealso [as_estimates_vcov()] to build the object from [prep_fit()] output,
#'   and [bind_estimates_vcov()] to combine the result with other objects.
#'
#' @family estimates_vcov objects
#'
#' @examples
#' # Two experiments on overlapping subjects: every subject takes the survey
#' # experiment, a random third also takes the lab experiment. The two effect
#' # estimates are correlated, but there is no single regression to read the
#' # covariance off, so bootstrap the design and use cov() of the replicates.
#' set.seed(123)
#' n <- 400
#' dat <- data.frame(
#'   Z_survey = rbinom(n, 1, 0.5),
#'   in_lab = rbinom(n, 1, 1 / 3)
#' )
#' dat$Z_lab <- ifelse(dat$in_lab == 1, rbinom(n, 1, 0.5), NA)
#' dat$Y_survey <- 0.2 * dat$Z_survey + rnorm(n)
#' dat$Y_lab <- 0.5 * dat$Z_lab + 0.6 * dat$Y_survey + rnorm(n)
#'
#' estimate_both <- function(d) {
#'   c(
#'     survey = coef(lm(Y_survey ~ Z_survey, data = d))[["Z_survey"]],
#'     lab = coef(lm(Y_lab ~ Z_lab, data = d[d$in_lab == 1, ]))[["Z_lab"]]
#'   )
#' }
#'
#' # Resample subjects, not rows within experiment, so the shared-sample
#' # correlation is what the replicates reproduce
#' boots <- t(replicate(200, estimate_both(dat[sample(n, n, replace = TRUE), ])))
#' V <- cov(boots)
#' point <- estimate_both(dat)
#'
#' estimates_df <- data.frame(
#'   study = "study_1",
#'   term = names(point),
#'   estimate = point,
#'   std.error = sqrt(diag(V))
#' )
#'
#' ev <- make_estimates_vcov(estimates_df, V)
#' ev
#' get_vcov(ev)
#'
#' @export
make_estimates_vcov <- function(estimates_df, vcov_matrix) {
  # --- Defensive checks ----
  if (!is.data.frame(estimates_df)) {
    rlang::abort("`estimates_df` must be a data frame or tibble.")
  }

  # Either representation is accepted and neither is converted: storage follows
  # the data. A block-diagonal vcov is overwhelmingly zeros and belongs sparse; a
  # bootstrapped cov() is genuinely dense and would only grow if forced sparse.
  if (!is.matrix(vcov_matrix) && !methods::is(vcov_matrix, "Matrix")) {
    rlang::abort("`vcov_matrix` must be a matrix or Matrix object.")
  }

  # Validate dimensions
  if (nrow(estimates_df) != nrow(vcov_matrix)) {
    rlang::abort(c(
      "Dimension mismatch between estimates and vcov matrix.",
      "i" = sprintf(
        "estimates_df has %d rows, vcov_matrix has %d rows",
        nrow(estimates_df), nrow(vcov_matrix)
      )
    ))
  }

  if (nrow(vcov_matrix) != ncol(vcov_matrix)) {
    rlang::abort(c(
      "vcov_matrix must be square.",
      "i" = sprintf(
        "vcov_matrix has dimensions %d x %d",
        nrow(vcov_matrix), ncol(vcov_matrix)
      )
    ))
  }

  # Repair floating-point asymmetry; error on anything larger (see symmetrize_vcov)
  vcov_matrix <- symmetrize_vcov(vcov_matrix)

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
    is.matrix(vcov) || methods::is(vcov, "Matrix"),
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

# ---- Storage-agnostic vcov arithmetic ----
#
# A block-diagonal vcov is overwhelmingly zeros (99.88% on the largest object in
# use), so it is a natural candidate for sparse storage. The helpers below do the
# handful of operations the package performs on a vcov in a way that is correct
# and allocation-frugal for either representation, so that changing how the
# matrix is stored cannot change any number the package reports.
#
# The trap each one avoids is a silent densification: an expression that is
# perfectly correct but materializes a full k-by-k dense temporary, which throws
# away the entire benefit while looking innocent.

# TRUE for a Matrix-package sparse matrix.
is_sparse_vcov <- function(x) methods::is(x, "sparseMatrix")

# Transpose. A bare `t()` inside this namespace dispatches to `base::t.default`
# for a Matrix object, because the package imports Matrix functions rather than
# its S4 methods, and that errors with "argument is not a matrix".
vcov_t <- function(x) if (is_sparse_vcov(x)) Matrix::t(x) else t(x)

# Coerce to a triplet form that stores *both* triangles. A symmetric matrix
# coerced with `as(V, "sparseMatrix")` becomes a symmetric class holding only one
# triangle, so scanning its stored values sees each off-diagonal entry once and
# misses the mirror. Going through "generalMatrix" first expands that, which is
# what makes the scans below agree with their dense equivalents.
to_general_triplet <- function(x) {
  methods::as(methods::as(x, "generalMatrix"), "TsparseMatrix")
}

# Locate non-finite cells. `is.finite(as.matrix(V))` would densify; in a sparse
# matrix only the *stored* values can be non-finite, since structural zeros are
# finite by construction, so the scan is over nnz rather than k^2 cells.
nonfinite_cells <- function(vcov) {
  if (is_sparse_vcov(vcov)) {
    trip <- to_general_triplet(vcov)
    bad <- !is.finite(trip@x)
    list(rows = trip@i[bad] + 1L, n_bad = sum(bad), n_cells = prod(dim(vcov)))
  } else {
    finite <- is.finite(vcov)
    idx <- which(!finite, arr.ind = TRUE)
    list(rows = idx[, "row"], n_bad = nrow(idx), n_cells = length(finite))
  }
}

# Count nonzero entries strictly above the diagonal, i.e. how many covariances
# the matrix actually carries. `sum(V[upper.tri(V)] != 0)` allocates a dense
# k-by-k logical; for the largest object in use that is 13.6 MB to answer a
# question the sparse structure already knows.
count_offdiag_nonzero <- function(vcov) {
  if (is_sparse_vcov(vcov)) {
    trip <- to_general_triplet(vcov)
    # i < j selects the strict upper triangle outright, so there is nothing to
    # halve. Halving a "both triangles" count is the obvious wrong move here and
    # silently reports half the covariances.
    sum(trip@i < trip@j & trip@x != 0)
  } else {
    sum(vcov[upper.tri(vcov)] != 0)
  }
}

# V |-> diag(s) V diag(s), i.e. V[i, j] * s[i] * s[j]. The direct spelling
# `V * tcrossprod(s)` is dense k-by-k in the multiplier alone (27.3 MB on the
# largest object), so scale rows and columns separately instead.
scale_vcov <- function(vcov, s) {
  out <- if (is_sparse_vcov(vcov)) {
    d <- Matrix::Diagonal(x = s)
    scaled <- d %*% vcov %*% d
    # diag(s) V diag(s) preserves symmetry, but the product loses the *symmetric
    # storage class*, which doubles the stored values. Restore it when the input
    # had it, so a sign flip does not silently double an object's footprint.
    # Conditional on the input's class rather than unconditional, so this cannot
    # mask a genuinely asymmetric matrix.
    if (methods::is(vcov, "symmetricMatrix")) {
      Matrix::forceSymmetric(scaled)
    } else {
      scaled
    }
  } else {
    # `V * s` scales row i by s[i]; transposing and repeating scales column j by
    # s[j]. No k-by-k temporary beyond the result itself.
    t(t(vcov * s) * s)   # dense branch only, so base t() is correct here
  }
  dimnames(out) <- dimnames(vcov)
  out
}

# Repair a vcov matrix's symmetry, or error if it is too asymmetric to be a
# valid covariance matrix.
#
# Block-diagonal assembly (Matrix::bdiag) and sandwich variance estimators can
# leave a vcov a few ulps off symmetric; that noise is averaged away silently.
# Asymmetry beyond `tol` (relative to the matrix scale) is not floating-point
# noise: it means the rows and columns are misaligned or a block is malformed,
# which would make the meta-analysis silently wrong, so it errors instead.
symmetrize_vcov <- function(vcov, tol = sqrt(.Machine$double.eps),
                            call = rlang::caller_env()) {
  if (length(vcov) == 0) {
    return(vcov)
  }

  # Check for non-finite entries before testing symmetry. A rank-deficient fit
  # can return finite coefficients alongside an NA/NaN covariance, which is how
  # such an estimate reaches this point: it looks usable but carries no
  # uncertainty. Without this branch both `asym` and `scale` come back NaN, the
  # symmetry condition evaluates to NA, and R aborts with "missing value where
  # TRUE/FALSE needed" -- an error that points at symmetry rather than at the
  # non-finite values that actually caused it.
  bad <- nonfinite_cells(vcov)
  if (bad$n_bad > 0) {
    ids <- rownames(vcov)
    where <- if (!is.null(ids)) {
      paste(unique(ids[bad$rows]), collapse = ", ")
    } else {
      paste(unique(bad$rows), collapse = ", ")
    }
    rlang::abort(
      c(
        "`vcov` contains non-finite values and cannot be a valid covariance matrix.",
        "i" = sprintf("%d of %d cells are NA, NaN, or infinite.",
                      bad$n_bad, bad$n_cells),
        "i" = sprintf("Affected rows/columns: %s.", where),
        "i" = paste("This usually means a rank-deficient fit returned",
                    "coefficients but no usable standard errors. Fix the fit or",
                    "drop the affected estimates rather than pooling them.")
      ),
      call = call
    )
  }

  asym <- max(abs(vcov - vcov_t(vcov)))
  scale <- max(abs(vcov))
  if (scale > 0 && asym > tol * scale) {
    rlang::abort(
      c(
        "`vcov` is not symmetric and cannot be a valid covariance matrix.",
        "i" = sprintf("max|V - t(V)| = %.3g exceeds the tolerance %.3g (tol * max|V|).",
                      asym, tol * scale),
        "i" = "Check for misaligned rows/columns or a malformed vcov block in the input."
      ),
      call = call
    )
  }
  (vcov + vcov_t(vcov)) / 2
}

#' Combine estimates_vcov objects
#'
#' @description
#' Row-binds the estimates of two or more `estimates_vcov` objects and assembles
#' their variance-covariance matrices into a single block-diagonal matrix, with
#' zero covariance between objects. Use it when studies were prepared into
#' separate `estimates_vcov` objects but should be meta-analyzed together.
#'
#' This is not a plain row-bind: the block-diagonal vcov is rebuilt so that it
#' stays synchronized with the stacked estimates, and the `id` column is
#' renumbered across the combined object.
#'
#' @param ... Two or more `estimates_vcov` objects, or a single list of them.
#'
#' @return A combined `estimates_vcov` object.
#'
#' @examplesIf requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(dplyr)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat_a <- data.frame(Z = complete_ra(80, num_arms = 2), Y = rnorm(80))
#' dat_b <- data.frame(Z = complete_ra(120, num_arms = 3), Y = rnorm(120))
#'
#' ev_a <- as_estimates_vcov(bind_rows(
#'   study_1 = prep_fit(lm_robust(Y ~ Z, dat_a), term = "ZT2"),
#'   .id = "study"
#' ))
#' ev_b <- as_estimates_vcov(bind_rows(
#'   study_2 = prep_fit(lm_robust(Y ~ Z, dat_b), term = c("ZT2", "ZT3")),
#'   .id = "study"
#' ))
#'
#' # One object, block-diagonal vcov, id renumbered 1..n
#' bind_estimates_vcov(ev_a, ev_b)
#'
#' @seealso [estimates_vcov] for the object's structure, and
#'   [rescale_estimates_vcov()] to align estimate signs before or after combining.
#'
#' @family estimates_vcov objects
#' @importFrom dplyr bind_rows
#' @importFrom Matrix bdiag
#' @importFrom rlang abort
#' @export
bind_estimates_vcov <- function(...) {
  objs <- list(...)
  # Allow a single list of objects: bind_estimates_vcov(list(ev1, ev2))
  if (length(objs) == 1 && is.list(objs[[1]]) &&
      !inherits(objs[[1]], "estimates_vcov")) {
    objs <- objs[[1]]
  }

  if (length(objs) == 0) {
    rlang::abort("No `estimates_vcov` objects supplied.")
  }
  if (!all(vapply(objs, inherits, logical(1), "estimates_vcov"))) {
    rlang::abort("All inputs to `bind_estimates_vcov()` must be estimates_vcov objects.")
  }
  if (length(objs) == 1) {
    return(objs[[1]])
  }

  # Row-bind estimates, dropping the per-object id (renumbered in the constructor)
  estimates <- dplyr::bind_rows(lapply(objs, function(o) {
    e <- o$estimates
    e[["id"]] <- NULL
    e
  }))

  # Block-diagonal assembly: independent across objects (zero cross-covariance)
  vcov <- Matrix::bdiag(lapply(objs, function(o) o$vcov))
  vcov <- symmetrize_vcov(vcov)

  new_estimates_vcov(estimates, vcov)
}

#' Sign-flip or rescale an estimates_vcov object
#'
#' @description
#' Multiply each estimate by a per-row factor and update the variance-covariance
#' matrix to match, keeping the object internally consistent. Use it to align the
#' sign of estimates across studies (`by` of `+1` / `-1`) or to change units
#' (e.g. `by = 100` for percentage points).
#'
#' This is the correct way to transform estimate *values*. The dplyr methods keep
#' the vcov row-aligned (subsetting, reordering) but never transform it, so
#' `mutate(estimate = -estimate)` would flip the estimates while leaving the vcov
#' (and its cross-study covariances) inconsistent. `rescale_estimates_vcov()`
#' applies \eqn{V \mapsto \mathrm{diag}(s)\, V\, \mathrm{diag}(s)}, so the
#' covariances stay valid, including the sign of cross-covariances under a partial
#' sign flip. `std.error`, `statistic`, and the confidence bounds are updated to
#' match when present.
#'
#' @param ev An `estimates_vcov` object.
#' @param by A per-estimate multiplier: a bare column name, an expression
#'   evaluated in the estimates, or a numeric vector of length 1 (recycled) or
#'   `nrow(estimates)`. Use `+1` / `-1` to flip signs, positive values to rescale.
#'
#' @return An `estimates_vcov` object with `estimate` (and `std.error`,
#'   `statistic`, `conf.low`, `conf.high` when present) and the vcov rescaled.
#'
#' @examplesIf requireNamespace("randomizr", quietly = TRUE) && requireNamespace("estimatr", quietly = TRUE)
#' library(dplyr)
#' library(randomizr)
#' library(estimatr)
#'
#' set.seed(123)
#' dat <- data.frame(Z = complete_ra(120, num_arms = 3), Y = rnorm(120))
#' ev <- as_estimates_vcov(bind_rows(
#'   study_1 = prep_fit(lm_robust(Y ~ Z, dat), term = c("ZT2", "ZT3")),
#'   .id = "study"
#' ))
#'
#' # Flip the sign of the first arm only; the cross-covariance sign updates too
#' ev |> rescale_estimates_vcov(by = if_else(term == "ZT2", -1, 1))
#'
#' # Rescale to percentage points
#' ev |> rescale_estimates_vcov(by = 100)
#'
#' @seealso [dplyr-methods], which keep the vcov row-aligned but never transform
#'   it, and [estimates_vcov] for what the object guarantees.
#'
#' @family estimates_vcov objects
#' @importFrom rlang enquo eval_tidy abort
#' @export
rescale_estimates_vcov <- function(ev, by) {
  if (!inherits(ev, "estimates_vcov")) {
    rlang::abort("`ev` must be an estimates_vcov object.")
  }
  est <- ev$estimates
  s <- rlang::eval_tidy(rlang::enquo(by), data = est)
  if (length(s) == 1L) s <- rep(s, nrow(est))
  if (!is.numeric(s) || length(s) != nrow(est)) {
    rlang::abort("`by` must be numeric of length 1 or nrow(estimates).")
  }

  est$estimate <- s * est$estimate
  if ("std.error" %in% names(est)) est$std.error <- abs(s) * est$std.error
  if ("statistic" %in% names(est)) est$statistic <- sign(s) * est$statistic
  if (all(c("conf.low", "conf.high") %in% names(est))) {
    lo <- s * est$conf.low
    hi <- s * est$conf.high
    est$conf.low  <- pmin(lo, hi)
    est$conf.high <- pmax(lo, hi)
  }

  # V |-> diag(s) V diag(s), i.e. V[i,j] * s[i] * s[j]
  vcov <- scale_vcov(ev$vcov, s)

  new_estimates_vcov(est, vcov, row_map = ev$row_map)
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
#' @family component accessors
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
    rlang::abort(c(
      "Input must contain a list-column named `tidy_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    ))
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
#' @family component accessors
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
    rlang::abort(c(
      "Input must contain a list-column named `glance_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    ))
  }

  x |>
    dplyr::select(-dplyr::any_of(c("tidy_obj", "vcov_obj"))) |>
    tidyr::unnest("glance_obj")
}

#' @export
get_glance_df.estimates_vcov <- function(x, ...) {
  rlang::warn(c(
    "glance information is not stored in estimates_vcov objects.",
    "i" = "Extract glance from prepped_fits before creating estimates_vcov."
  ))
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
#' @family component accessors
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
    rlang::abort(c(
      "Input must contain a list-column named `vcov_obj`.",
      "i" = "Did you pass the result of `prep_fit()` (or a bind_rows() of them)?"
    ))
  }

  x |>
    dplyr::pull("vcov_obj") |>
    Matrix::bdiag()
}

#' @export
get_vcov.estimates_vcov <- function(x, ...) {
  x$vcov
}
