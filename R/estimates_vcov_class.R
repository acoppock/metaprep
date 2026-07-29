#' The estimates_vcov object
#'
#' @description
#' An `estimates_vcov` object holds a set of coefficient estimates together with
#' the variance-covariance matrix that describes their dependence, and keeps the
#' two aligned as you reshape them. It is the object the rest of the package is
#' built around: [as_estimates_vcov()] and [make_estimates_vcov()] create one,
#' the [dplyr-methods] reshape one, and [rma_mv_helper()] pools one.
#'
#' The problem it solves is bookkeeping. A meta-analysis of multi-arm trials
#' needs a block-diagonal vcov whose row `i` corresponds to estimate `i`, and
#' every filter, sort, or subset of the estimates has to be applied to the matrix
#' in the same way. Doing that by hand is where a meta-analysis goes silently
#' wrong: nothing about a misaligned matrix looks wrong, and the pooled result is
#' simply the wrong number.
#'
#' @section Structure:
#' The object is a list of three components:
#'
#' \describe{
#'   \item{`estimates`}{A tibble of estimates, one row each, with an `id` column
#'     first. Carries whatever columns [prep_fit()] produced (`term`, `estimate`,
#'     `std.error`, and so on) plus any you have added.}
#'   \item{`vcov`}{A square symmetric matrix, one row and column per estimate, in
#'     the same order as the rows of `estimates`, with `dimnames` equal to the
#'     `id` values.}
#'   \item{`row_map`}{Internal bookkeeping. See "Public interface" below.}
#' }
#'
#' @section Public interface:
#' `estimates` and `vcov` are part of the public interface. Read them however
#' suits the code you are writing: `ev$estimates` and `ev$vcov` directly, or
#' [get_estimates_df()] and [get_vcov()]. Both will keep working.
#'
#' What is guaranteed about `vcov` is its *content and shape*, not its storage
#' class: square, symmetric, finite, one row and column per estimate in the same
#' order as `estimates`, `dimnames` equal to `id`. Ordinary matrix operations
#' (`V[i, j]`, `V[idx, idx]`, `diag(V)`, `upper.tri(V)`, `w %*% V %*% w`) are the
#' supported way to use it, and they behave the same whether the matrix is stored
#' densely or sparsely. Code that tests the storage class itself, for instance
#' with `is.matrix()`, is relying on something the object does not promise.
#'
#' `row_map` is **internal** and may change without notice. It records which rows
#' of a parent object the current rows came from, but the verbs do not agree on
#' what "parent" means: `filter()` and the filtering joins set it to positions
#' within the object they were handed, while `arrange()` composes it through. Do
#' not build on it. Use `id` when you need a stable per-estimate label; it is
#' preserved through subsetting for exactly that purpose.
#'
#' @section The id column:
#' Every object carries an `id` column, added when it is built, and the vcov's
#' `dimnames` match it. `id` is what links an estimate to its row and column of
#' the matrix, and it is preserved through subsetting, so a filtered object keeps
#' the ids it had rather than being renumbered from one. That is what makes `id`
#' usable as a random-effect grouping term: `random = ~ 1 | id` gives each
#' estimate its own random effect. ([bind_estimates_vcov()] is the exception: it
#' renumbers `id` across the combined object, since the inputs each started
#' from one.)
#'
#' @section What stays synchronized, and what does not:
#' The dplyr methods keep the vcov **row-aligned**. Dropping or reordering
#' estimates drops or reorders the matching rows and columns of the matrix, so
#' `filter()`, `slice()`, `arrange()`, `semi_join()`, and `anti_join()` are all
#' safe. Verbs that cannot change the row set (`mutate()`, `select()`, `rename()`,
#' `relocate()`) leave the matrix untouched. Verbs that would add or drop rows
#' unpredictably (`inner_join()`, `full_join()`, `right_join()`) are refused
#' rather than silently desynchronizing the object.
#'
#' What the dplyr methods never do is **transform** the matrix. `mutate()` can
#' change the values in the `estimate` column while leaving the vcov exactly as it
#' was, so `mutate(estimate = -estimate)` produces an object whose covariances no
#' longer describe its estimates. Use [rescale_estimates_vcov()] to sign-flip or
#' rescale, which updates the matrix to match.
#'
#' @section Guarantees:
#' Every object that gets built satisfies three conditions, checked at
#' construction: the estimates and the vcov have the same number of rows; the
#' vcov is square and symmetric (asymmetry beyond floating-point noise is an
#' error, not something to average away); and the vcov is finite. A non-finite
#' covariance usually means a rank-deficient fit returned coefficients without
#' usable standard errors, and such an estimate must not be pooled as if it
#' carried uncertainty.
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(1)
#' dat_1 <- data.frame(Y = rnorm(60), Z = factor(rep(c("T0", "T1"), each = 30)))
#' dat_2 <- data.frame(Y = rnorm(90), Z = factor(rep(c("T0", "T1", "T2"), each = 30)))
#'
#' ev <- as_estimates_vcov(bind_rows(
#'   study_1 = prep_fit(lm(Y ~ Z, dat_1), term = "ZT1"),
#'   study_2 = prep_fit(lm(Y ~ Z, dat_2), term = c("ZT1", "ZT2")),
#'   .id = "study"
#' ))
#' ev
#'
#' # The id column indexes the vcov
#' get_estimates_df(ev)$id
#' rownames(get_vcov(ev))
#'
#' # Study 2's two arms covary; study 1 is independent of both
#' round(get_vcov(ev), 5)
#'
#' # Subsetting keeps the two in step, and keeps the original ids
#' ev_2 <- filter(ev, study == "study_2")
#' get_estimates_df(ev_2)$id
#' dim(get_vcov(ev_2))
#'
#' @seealso [as_estimates_vcov()] and [make_estimates_vcov()] to build one,
#'   [dplyr-methods] for the verbs it supports, and [rma_mv_helper()] to pool it.
#' @family estimates_vcov objects
#' @name estimates_vcov
#' @aliases estimates_vcov-class print.estimates_vcov
#'   as.data.frame.estimates_vcov as_tibble.estimates_vcov
NULL
