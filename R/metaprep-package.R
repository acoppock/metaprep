#' @keywords internal
#'
#' @description
#' Meta-analyzing effect estimates from multi-arm trials means accounting for the
#' dependence that arises when several treatment arms are compared against a
#' shared control group. [metafor::rma.mv()] implements the methods: you hand it a
#' block-diagonal variance-covariance matrix that encodes the dependence. The
#' awkward part is building that matrix and keeping it aligned with the estimates
#' as you filter, group, and subset them. metaprep does that bookkeeping.
#'
#' @section Where to start:
#' The workflow has four steps, and the whole package is organized around them.
#'
#' 1. **Prepare each fit.** [prep_fit()] runs `tidy()`, `glance()`, and `vcov()`
#'    on a fitted model and keeps the terms you name, returning them as
#'    list-columns of a one-row tibble. Bind one per study with
#'    `dplyr::bind_rows(.id = "study")`.
#' 2. **Build the object.** [as_estimates_vcov()] turns those bound fits into an
#'    [estimates_vcov] object: the estimates and their block-diagonal vcov, held
#'    together. When the covariances do not come from a single regression, for
#'    example bootstrapped across experiments that share subjects, build it with
#'    [make_estimates_vcov()] instead.
#' 3. **Reshape it.** dplyr verbs work on the object and keep the vcov aligned
#'    (see [dplyr-methods]). To change estimate *values*, use
#'    [rescale_estimates_vcov()], which transforms the vcov to match. To stack
#'    objects prepared separately, use [bind_estimates_vcov()].
#' 4. **Pool it.** [rma_mv_helper()] reads the estimates and vcov straight off the
#'    object and passes them to [metafor::rma.mv()], optionally with
#'    cluster-robust standard errors.
#'
#' [get_estimates_df()], [get_glance_df()], and [get_vcov()] pull the components
#' back out at any point.
#'
#' @section What the package guards against:
#' Two failures in this workflow are silent, and both would make a
#' meta-analysis wrong with no visible symptom, so metaprep errors rather than
#' guessing:
#'
#' - **A vcov that cannot be a covariance matrix.** Asymmetry beyond
#'   floating-point noise means rows and columns are misaligned; non-finite
#'   entries mean a rank-deficient fit returned coefficients without usable
#'   standard errors. Both are rejected when the object is built.
#' - **An estimate that cannot enter a pool.** A non-finite estimate is rejected
#'   by [rma_mv_helper()] and [rma_uni_helper()], because `metafor` would drop it
#'   silently and return a fit holding fewer estimates than the object.
#'
#' In each case which estimates to drop is an analyst's decision, so the package
#' stops and leaves it to you rather than choosing quietly.
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
#' # Subsetting the estimates subsets the vcov with them
#' dim(get_vcov(ev))
#' dim(get_vcov(filter(ev, study == "study_2")))
"_PACKAGE"
