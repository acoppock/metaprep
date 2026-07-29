# metaprep 0.3.1

* New `?estimates_vcov` help page documenting the object the package is built
  around. It had none: the central noun of the package was undocumented, so
  nothing said what the components are, what the `id` column is for, which dplyr
  verbs keep the vcov aligned and which are refused, or what the object
  guarantees at construction. `?metaprep` is also new, giving the four-step
  workflow and the two silent failures the package refuses to guess about.
* `?estimates_vcov` gains a "Public interface" section settling what callers may
  rely on. `estimates` and `vcov` are public: read them as `ev$estimates` /
  `ev$vcov` or through [get_estimates_df()] / [get_vcov()], whichever suits, and
  both will keep working. What is guaranteed about `vcov` is its content and
  shape (square, symmetric, finite, one row and column per estimate in the same
  order, `dimnames` equal to `id`), not its storage class, so ordinary matrix
  operations are the supported way to use it and `is.matrix()` is not. `row_map`
  is documented as internal, since the verbs do not agree on what its "parent"
  means: `filter()` and the filtering joins set it to positions within the object
  they were handed while `arrange()` composes it through. Use `id` for a stable
  per-estimate label.
* The S3 methods are reachable by name. `?filter.estimates_vcov`,
  `?print.estimates_vcov`, `?nest_by.estimates_vcov` and the rest previously
  returned "no documentation": the `dplyr-methods` page carried no aliases, so a
  caller who knew a method's name could not find its page.
* `dplyr-methods` gains runnable examples, documented arguments, and a `@return`
  section. It is the page that carries the `mutate(estimate = -estimate)` desync
  warning and it had nothing runnable beside the prose. The examples now show the
  trap and its fix on the one entry where it is visible, the within-study
  covariance, using a partial sign flip so the covariance changes sign.
* `@family` tags group the exports into `estimates_vcov objects`, `component
  accessors`, and `meta-analysis wrappers`, so every page now generates a See
  Also block pointing at its siblings. There were none before, across ten
  exports. `prep_fit()` gains an explicit `@seealso` to the same effect.
* New examples use base `lm()` rather than `randomizr` plus `estimatr`, so they
  run without any suggested package installed.

* `rma_uni_helper()` no longer discards covariances silently. `rma.uni()` takes
  only the variances, so calling it on an object whose vcov has nonzero
  off-diagonals throws away the dependence the package exists to carry and
  returns a standard error that is too small: on a five-estimate two-study
  object, 0.100 against `rma_mv_helper()`'s 0.152. The docs already said to use
  `rma_mv_helper()` for correlated estimates, but nothing fired at runtime. When
  `vi` is not supplied and the vcov has nonzero off-diagonal entries,
  `rma_uni_helper()` now warns (class `"metaprep_discarded_covariance"`), names
  how many covariances were dropped, and points at `rma_mv_helper()`. Supplying
  `vi` explicitly silences it and returns an identical fit, which is the way to
  say the univariate fit is what you meant. Objects with no covariances (the
  ordinary univariate case) are unaffected.
* `rma_mv_helper()` and `rma_uni_helper()` now error when an estimate entering
  the pool is `NA`, `NaN`, or infinite. `metafor` drops such rows with a warning
  and returns a fit whose `k` is smaller than the object, so anything joining a
  per-estimate quantity back onto the estimates (`weights()`, `resid()`) silently
  misaligns. The error names the count and the affected `id`s. This is the mirror
  of the non-finite `vcov` guard added in 0.2.x, and it follows the same
  reasoning: an estimate that cannot carry its own weight must not enter a pooled
  fit silently, and which estimates to drop is the analyst's call. Verified
  against 115 `estimates_vcov` objects across the meta-reanalysis projects, none
  of which has a non-finite estimate, so no existing pipeline changes.

* Fourteen error and warning messages were silently dropping their guidance
  bullet. `rlang::abort(msg, "i" = hint)` passes the hint into `...`, where rlang
  stores it as a condition field and never prints it; the bullets only render
  when they are part of the message vector, as `rlang::abort(c(msg, "i" = hint))`.
  So `as_estimates_vcov(some_data_frame)` reported only "Input must contain
  list-columns named `tidy_obj` and `vcov_obj`." and swallowed "Did you pass the
  result of `prep_fit()`?", which is the hint a first-time caller most needs. All
  fourteen now print their bullet: the four "did you pass `prep_fit()` output"
  hints, the four dimension and squareness reports, the three
  missing-package install lines, `get_glance_df()`'s pointer to extract glance
  before building the object, and `prep_fit()`'s missing-`tidy()`/`glance()`/
  `vcov()`-method hints. A new `test-error-messages.R` asserts on the hint text
  rather than the headline, so the defect class cannot return silently.
* The vignette's bootstrap section pooled with `random = ~ 1 | study` where every
  other chunk uses `random = ~ 1 | id`, so the one place the reader meets a
  combined object silently switched to a different model (a study-level intercept
  over five levels rather than an estimate-level random effect over eight,
  moving tau^2 from 0.006 to 0.020 and the pooled estimate from 0.223 to 0.238).
  Worse, the two bootstrapped rows are one study sharing subjects, and
  `~ 1 | study` entered them as two independent studies. Now `~ 1 | id`
  throughout.

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
