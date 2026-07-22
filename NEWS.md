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
