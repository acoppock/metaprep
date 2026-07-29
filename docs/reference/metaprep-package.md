# metaprep: Tidy Preparation of Dependent Effect Estimates for Meta-Analysis

Meta-analyzing effect estimates from multi-arm trials means accounting
for the dependence that arises when several treatment arms are compared
against a shared control group.
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
implements the methods: you hand it a block-diagonal variance-covariance
matrix that encodes the dependence. The awkward part is building that
matrix and keeping it aligned with the estimates as you filter, group,
and subset them. metaprep does that bookkeeping.

## Where to start

The workflow has four steps, and the whole package is organized around
them.

1.  **Prepare each fit.**
    [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
    runs [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
    [`glance()`](https://generics.r-lib.org/reference/glance.html), and
    [`vcov()`](https://rdrr.io/r/stats/vcov.html) on a fitted model and
    keeps the terms you name, returning them as list-columns of a
    one-row tibble. Bind one per study with
    `dplyr::bind_rows(.id = "study")`.

2.  **Build the object.**
    [`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
    turns those bound fits into an
    [estimates_vcov](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
    object: the estimates and their block-diagonal vcov, held together.
    When the covariances do not come from a single regression, for
    example bootstrapped across experiments that share subjects, build
    it with
    [`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
    instead.

3.  **Reshape it.** dplyr verbs work on the object and keep the vcov
    aligned (see
    [dplyr-methods](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)).
    To change estimate *values*, use
    [`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md),
    which transforms the vcov to match. To stack objects prepared
    separately, use
    [`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md).

4.  **Pool it.**
    [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
    reads the estimates and vcov straight off the object and passes them
    to
    [`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html),
    optionally with cluster-robust standard errors.

[`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md),
[`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md),
and
[`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md)
pull the components back out at any point.

## What the package guards against

Two failures in this workflow are silent, and both would make a
meta-analysis wrong with no visible symptom, so metaprep errors rather
than guessing:

- **A vcov that cannot be a covariance matrix.** Asymmetry beyond
  floating-point noise means rows and columns are misaligned; non-finite
  entries mean a rank-deficient fit returned coefficients without usable
  standard errors. Both are rejected when the object is built.

- **An estimate that cannot enter a pool.** A non-finite estimate is
  rejected by
  [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  and
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md),
  because `metafor` would drop it silently and return a fit holding
  fewer estimates than the object.

In each case which estimates to drop is an analyst's decision, so the
package stops and leaves it to you rather than choosing quietly.

## See also

Useful links:

- <https://alexandercoppock.com/metaprep/>

- <https://github.com/acoppock/metaprep>

- Report bugs at <https://github.com/acoppock/metaprep/issues>

## Author

**Maintainer**: Alex Coppock <acoppock@gmail.com>

Authors:

- Alex Coppock <acoppock@gmail.com>

## Examples

``` r
library(dplyr)

set.seed(1)
dat_1 <- data.frame(Y = rnorm(60), Z = factor(rep(c("T0", "T1"), each = 30)))
dat_2 <- data.frame(Y = rnorm(90), Z = factor(rep(c("T0", "T1", "T2"), each = 30)))

ev <- as_estimates_vcov(bind_rows(
  study_1 = prep_fit(lm(Y ~ Z, dat_1), term = "ZT1"),
  study_2 = prep_fit(lm(Y ~ Z, dat_2), term = c("ZT1", "ZT2")),
  .id = "study"
))
ev
#> <estimates_vcov>
#> # 3 estimates with 3x3 vcov matrix
#> 
#> # A tibble: 3 × 7
#>   id    study   term  estimate std.error statistic p.value
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     study_1 ZT1    0.0503      0.223    0.226   0.822 
#> 2 2     study_2 ZT1    0.00305     0.238    0.0128  0.990 
#> 3 3     study_2 ZT2   -0.440       0.238   -1.85    0.0679

# Subsetting the estimates subsets the vcov with them
dim(get_vcov(ev))
#> [1] 3 3
dim(get_vcov(filter(ev, study == "study_2")))
#> [1] 2 2
```
