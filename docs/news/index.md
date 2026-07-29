# Changelog

## metaprep 0.3.1

- [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  no longer discards covariances silently.
  [`rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
  takes only the variances, so calling it on an object whose vcov has
  nonzero off-diagonals throws away the dependence the package exists to
  carry and returns a standard error that is too small: on a
  five-estimate two-study object, 0.100 against
  [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)’s
  0.152. The docs already said to use
  [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  for correlated estimates, but nothing fired at runtime. When `vi` is
  not supplied and the vcov has nonzero off-diagonal entries,
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  now warns (class `"metaprep_discarded_covariance"`), names how many
  covariances were dropped, and points at
  [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md).
  Supplying `vi` explicitly silences it and returns an identical fit,
  which is the way to say the univariate fit is what you meant. Objects
  with no covariances (the ordinary univariate case) are unaffected.

- [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  and
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  now error when an estimate entering the pool is `NA`, `NaN`, or
  infinite. `metafor` drops such rows with a warning and returns a fit
  whose `k` is smaller than the object, so anything joining a
  per-estimate quantity back onto the estimates
  ([`weights()`](https://rdrr.io/r/stats/weights.html),
  [`resid()`](https://rdrr.io/r/stats/residuals.html)) silently
  misaligns. The error names the count and the affected `id`s. This is
  the mirror of the non-finite `vcov` guard added in 0.2.x, and it
  follows the same reasoning: an estimate that cannot carry its own
  weight must not enter a pooled fit silently, and which estimates to
  drop is the analyst’s call. Verified against 115 `estimates_vcov`
  objects across the meta-reanalysis projects, none of which has a
  non-finite estimate, so no existing pipeline changes.

- Fourteen error and warning messages were silently dropping their
  guidance bullet. `rlang::abort(msg, "i" = hint)` passes the hint into
  `...`, where rlang stores it as a condition field and never prints it;
  the bullets only render when they are part of the message vector, as
  `rlang::abort(c(msg, "i" = hint))`. So
  `as_estimates_vcov(some_data_frame)` reported only “Input must contain
  list-columns named `tidy_obj` and `vcov_obj`.” and swallowed “Did you
  pass the result of
  [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)?”,
  which is the hint a first-time caller most needs. All fourteen now
  print their bullet: the four “did you pass
  [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
  output” hints, the four dimension and squareness reports, the three
  missing-package install lines,
  [`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md)’s
  pointer to extract glance before building the object, and
  [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)’s
  missing-[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`glance()`](https://generics.r-lib.org/reference/glance.html)/
  [`vcov()`](https://rdrr.io/r/stats/vcov.html)-method hints. A new
  `test-error-messages.R` asserts on the hint text rather than the
  headline, so the defect class cannot return silently.

- The vignette’s bootstrap section pooled with `random = ~ 1 | study`
  where every other chunk uses `random = ~ 1 | id`, so the one place the
  reader meets a combined object silently switched to a different model
  (a study-level intercept over five levels rather than an
  estimate-level random effect over eight, moving tau^2 from 0.006 to
  0.020 and the pooled estimate from 0.223 to 0.238). Worse, the two
  bootstrapped rows are one study sharing subjects, and `~ 1 | study`
  entered them as two independent studies. Now `~ 1 | id` throughout.

## metaprep 0.3.0

- **Breaking:** `estimates_vcov_from_pieces()` is renamed
  [`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md).
  No deprecated alias is kept, since the package has not been released.
  Rename call sites; the arguments and behavior are unchanged.
- [`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
  is documented as the entry point for estimates whose covariances do
  not come from a single regression – most often experiments run on
  overlapping samples, where the covariance between their estimates
  comes from bootstrapping the design and taking
  [`cov()`](https://rdrr.io/r/stats/cor.html) of the replicate
  estimates. Previously it was described only as a way to recombine the
  output of
  [`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md)
  and
  [`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md),
  which undersold it. The docs now state that the vcov is matched to the
  estimates by position and that any dimnames on the matrix are
  discarded, and recommend supplying `std.error` so
  [`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
  has standard errors to rescale. The vignette gains a “Correlated
  estimates with no single regression” section working the
  overlapping-samples bootstrap end to end, through
  [`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md)
  and
  [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md).

## metaprep 0.2.3

- [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)’s
  `term` argument now accepts tidyselect expressions matched against the
  model’s term names, e.g. `prep_fit(fit, starts_with("Z_party"))`, so
  callers no longer hand-build a coefficient-name vector with
  `grep(..., names(coef(fit)))`. A treatment’s main effect can be taken
  while dropping its interactions with tidyselect’s own set algebra,
  e.g. `matches("^Z_treated$")` or
  `starts_with("Z_treated") & !contains(":")`. A character vector still
  works exactly as before (exact names, or regex with
  `match = "regex"`).

## metaprep 0.2.2

- New
  [`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
  sign-flips or rescales the estimates of an `estimates_vcov` object and
  updates the vcov to match (`diag(s) V diag(s)`), keeping the object
  consistent. This is the correct way to flip signs (including a partial
  flip of some arms) or change units; the dplyr methods keep the vcov
  row-aligned but never transform it, so `mutate(estimate = -estimate)`
  alone would leave the vcov inconsistent. The `dplyr-methods` docs now
  note this.

## metaprep 0.2.1

- The `cluster` argument of
  [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  and
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  now accepts a bare column name, a string-named column via
  `.data[[var]]`, or an external vector (previously only a bare column
  name worked). It is captured with
  [`rlang::enquo()`](https://rlang.r-lib.org/reference/enquo.html), so
  it composes inside wrapper functions that pass the clustering variable
  programmatically.

## metaprep 0.2.0

- [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  and
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  gain a `cluster` argument (a bare column name evaluated in the
  estimates, like `yi`) and a `clubSandwich` argument. When `cluster` is
  supplied the fit is wrapped in \[metafor::robust()\], so
  `rma_mv_helper(ev, yi = estimate, random = ~ 1 | id, cluster = study)`
  returns CR2 cluster-robust standard errors in one step. Defaults are
  unchanged (`cluster = NULL` returns the ordinary fit).
- New
  [`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md)
  combines two or more `estimates_vcov` objects into one, stacking the
  estimates and assembling a block-diagonal vcov (zero covariance
  between objects), with `id` renumbered across the result.
- [`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
  and `estimates_vcov_from_pieces()` now repair floating-point asymmetry
  in the vcov and error on asymmetry beyond a relative tolerance, which
  would indicate a malformed (non-covariance) matrix.
- [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  and
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  now error if a `mods` formula references a column that is not on the
  `estimates_vcov` object, so a moderator that never made it onto the
  object fails loudly instead of silently producing an empty
  meta-regression. The vignette gains sections on cluster-robust
  standard errors and meta-regression.

## metaprep 0.1.0

- First release.
- [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
  extracts tidy estimates, a
  [`glance()`](https://generics.r-lib.org/reference/glance.html)
  summary, and the relevant variance-covariance submatrix from a fitted
  model, with exact or regex term matching and multivariate-model
  handling.
- [`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
  and `estimates_vcov_from_pieces()` build an `estimates_vcov` object
  that keeps coefficient estimates and their block-diagonal vcov matrix
  synchronized.
- dplyr methods
  ([`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html),
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html), and
  the join verbs) operate on `estimates_vcov` objects while keeping the
  vcov matrix in sync.
- [`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md),
  [`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md),
  and
  [`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md)
  pull the components back out of either a prepped-fits tibble or an
  `estimates_vcov` object.
- [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  and
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  wrap
  [`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
  and
  [`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html),
  reading the estimates and vcov directly from an `estimates_vcov`
  object.
