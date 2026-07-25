# metaprep 0.3.0

* **Breaking:** `estimates_vcov_from_pieces()` is renamed
  `make_estimates_vcov()`. No deprecated alias is kept, since the package has
  not been released. Rename call sites; the arguments and behavior are
  unchanged.
* `make_estimates_vcov()` is documented as the entry point for estimates whose
  covariances do not come from a single regression -- most often experiments run
  on overlapping samples, where the covariance between their estimates comes
  from bootstrapping the design and taking `cov()` of the replicate estimates.
  Previously it was described only as a way to recombine the output of
  `get_estimates_df()` and `get_vcov()`, which undersold it. The docs now state
  that the vcov is matched to the estimates by position and that any dimnames on
  the matrix are discarded, and recommend supplying `std.error` so
  `rescale_estimates_vcov()` has standard errors to rescale. The vignette gains
  a "Correlated estimates with no single regression" section working the
  overlapping-samples bootstrap end to end, through
  `bind_estimates_vcov()` and `rma_mv_helper()`.

# metaprep 0.2.3

* `prep_fit()`'s `term` argument now accepts tidyselect expressions matched
  against the model's term names, e.g. `prep_fit(fit, starts_with("Z_party"))`,
  so callers no longer hand-build a coefficient-name vector with
  `grep(..., names(coef(fit)))`. A treatment's main effect can be taken while
  dropping its interactions with tidyselect's own set algebra, e.g.
  `matches("^Z_treated$")` or `starts_with("Z_treated") & !contains(":")`. A
  character vector still works exactly as before (exact names, or regex with
  `match = "regex"`).

# metaprep 0.2.2

* New `rescale_estimates_vcov()` sign-flips or rescales the estimates of an
  `estimates_vcov` object and updates the vcov to match
  (`diag(s) V diag(s)`), keeping the object consistent. This is the correct way
  to flip signs (including a partial flip of some arms) or change units; the
  dplyr methods keep the vcov row-aligned but never transform it, so
  `mutate(estimate = -estimate)` alone would leave the vcov inconsistent. The
  `dplyr-methods` docs now note this.

# metaprep 0.2.1

* The `cluster` argument of `rma_mv_helper()` and `rma_uni_helper()` now accepts
  a bare column name, a string-named column via `.data[[var]]`, or an external
  vector (previously only a bare column name worked). It is captured with
  `rlang::enquo()`, so it composes inside wrapper functions that pass the
  clustering variable programmatically.

# metaprep 0.2.0

* `rma_mv_helper()` and `rma_uni_helper()` gain a `cluster` argument (a bare
  column name evaluated in the estimates, like `yi`) and a `clubSandwich`
  argument. When `cluster` is supplied the fit is wrapped in
  [metafor::robust()], so `rma_mv_helper(ev, yi = estimate, random = ~ 1 | id,
  cluster = study)` returns CR2 cluster-robust standard errors in one step.
  Defaults are unchanged (`cluster = NULL` returns the ordinary fit).
* New `bind_estimates_vcov()` combines two or more `estimates_vcov` objects into
  one, stacking the estimates and assembling a block-diagonal vcov (zero
  covariance between objects), with `id` renumbered across the result.
* `as_estimates_vcov()` and `estimates_vcov_from_pieces()` now repair
  floating-point asymmetry in the vcov and error on asymmetry beyond a relative
  tolerance, which would indicate a malformed (non-covariance) matrix.
* `rma_mv_helper()` and `rma_uni_helper()` now error if a `mods` formula
  references a column that is not on the `estimates_vcov` object, so a moderator
  that never made it onto the object fails loudly instead of silently producing
  an empty meta-regression. The vignette gains sections on cluster-robust
  standard errors and meta-regression.

# metaprep 0.1.0

* First release.
* `prep_fit()` extracts tidy estimates, a `glance()` summary, and the relevant
  variance-covariance submatrix from a fitted model, with exact or regex term
  matching and multivariate-model handling.
* `as_estimates_vcov()` and `estimates_vcov_from_pieces()` build an
  `estimates_vcov` object that keeps coefficient estimates and their
  block-diagonal vcov matrix synchronized.
* dplyr methods (`filter()`, `slice()`, `arrange()`, `mutate()`, `select()`,
  `rename()`, `nest_by()`, and the join verbs) operate on `estimates_vcov`
  objects while keeping the vcov matrix in sync.
* `get_estimates_df()`, `get_glance_df()`, and `get_vcov()` pull the components
  back out of either a prepped-fits tibble or an `estimates_vcov` object.
* `rma_mv_helper()` and `rma_uni_helper()` wrap `metafor::rma.mv()` and
  `metafor::rma.uni()`, reading the estimates and vcov directly from an
  `estimates_vcov` object.
