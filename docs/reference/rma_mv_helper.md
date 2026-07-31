# Run rma.mv on an estimates_vcov object

A convenience wrapper for
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
that automatically extracts the estimates data frame and
variance-covariance matrix from an estimates_vcov object.

This function passes all arguments directly to
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html),
but handles the `data` and `V` arguments automatically.

## Usage

``` r
rma_mv_helper(object, yi, V = NULL, cluster = NULL, clubSandwich = TRUE, ...)
```

## Arguments

- object:

  An estimates_vcov object

- yi:

  Bare column name of the estimates (e.g. `estimate`), or a two-sided
  formula `estimate ~ moderators`. The formula form is metafor's own: it
  is passed through unchanged, and `yi = estimate ~ x` fits what
  `yi = estimate, mods = ~ x` fits. Supplying both a formula and `mods`
  is an error, since metafor would read the moderators off the formula
  and ignore `mods`.

- V:

  Variance-covariance matrix (defaults to the vcov from object)

- cluster:

  Optional clustering variable for cluster-robust (sandwich) standard
  errors. May be a bare column name of the estimates (e.g.
  `cluster = study`), a string-named column via `.data[[var]]`, or an
  external vector. When supplied, the fit is passed to
  [`metafor::robust()`](https://wviechtb.github.io/metafor/reference/robust.html);
  when `NULL` (default) the ordinary model-based fit is returned.

- clubSandwich:

  Logical, passed to
  [`metafor::robust()`](https://wviechtb.github.io/metafor/reference/robust.html)
  when `cluster` is supplied. `TRUE` (default) requests CR2
  cluster-robust standard errors via the clubSandwich package; `FALSE`
  uses metafor's CR0 estimator.

- ...:

  Additional arguments passed to
  [`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html),
  such as `random`, `mods`, etc.

## Value

An object of class `rma.mv` as returned by
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html),
or, when `cluster` is supplied, a `robust.rma` object from
[`metafor::robust()`](https://wviechtb.github.io/metafor/reference/robust.html).

## Details

Estimates must be finite. `metafor` drops non-finite rows with a warning
and returns a fit whose `k` is smaller than the object, which silently
misaligns anything joining a per-estimate quantity such as
[`stats::weights()`](https://rdrr.io/r/stats/weights.html) back onto the
estimates, so `rma_mv_helper()` errors instead and leaves the choice of
which estimates to drop to you. The same reasoning governs the
non-finite `vcov` guard applied when the object is built.

## See also

[`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
when the estimates are genuinely independent, and
[estimates_vcov](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
for the object it reads from.

Other meta-analysis wrappers:
[`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)

## Examples

``` r
library(dplyr)
library(randomizr)
library(estimatr)

set.seed(123)
dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
dat_2 <- data.frame(Z = complete_ra(100, num_arms = 3), Y = rnorm(100))
dat_3 <- data.frame(Z = complete_ra(200, num_arms = 4), Y = rnorm(200))

fit_1 <- lm_robust(Y ~ Z, data = dat_1)
fit_2 <- lm_robust(Y ~ Z, data = dat_2)
fit_3 <- lm_robust(Y ~ Z, data = dat_3)

prepped_fits <- bind_rows(
  study_1 = prep_fit(fit_1, term = "ZT2"),
  study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
  study_3 = prep_fit(fit_3, term = c("ZT2", "ZT3", "ZT4")),
  .id = "study"
)
ev <- as_estimates_vcov(prepped_fits)

ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#> 
#> Multivariate Meta-Analysis Model (k = 6; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      6     no      id 
#> 
#> Test for Heterogeneity:
#> Q(df = 5) = 4.6339, p-val = 0.4622
#> 
#> Model Results:
#> 
#> estimate      se     zval    pval    ci.lb   ci.ub    
#>  -0.0479  0.1189  -0.4028  0.6871  -0.2808  0.1851    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
ev |> rma_mv_helper(yi = estimate, mods = ~ study, random = ~ 1 | id)
#> 
#> Multivariate Meta-Analysis Model (k = 6; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      6     no      id 
#> 
#> Test for Residual Heterogeneity:
#> QE(df = 3) = 0.3102, p-val = 0.9581
#> 
#> Test of Moderators (coefficients 2:3):
#> QM(df = 2) = 4.3237, p-val = 0.1151
#> 
#> Model Results:
#> 
#>               estimate      se     zval    pval    ci.lb   ci.ub    
#> intrcpt        -0.4518  0.2761  -1.6363  0.1018  -0.9931  0.0894    
#> studystudy_2    0.7482  0.3621   2.0662  0.0388   0.0385  1.4579  * 
#> studystudy_3    0.3792  0.3187   1.1898  0.2341  -0.2455  1.0040    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
ev |>
  filter(study != "study_1") |>
  rma_mv_helper(yi = estimate, random = ~ 1 | id)
#> 
#> Multivariate Meta-Analysis Model (k = 5; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      5     no      id 
#> 
#> Test for Heterogeneity:
#> Q(df = 4) = 2.0072, p-val = 0.7344
#> 
#> Model Results:
#> 
#> estimate      se    zval    pval    ci.lb   ci.ub    
#>   0.0440  0.1317  0.3340  0.7384  -0.2141  0.3020    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 

# Cluster-robust (CR2) standard errors in one step (needs clubSandwich):
if (requireNamespace("clubSandwich", quietly = TRUE)) {
  ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id, cluster = study)
}
#> Registered S3 method overwritten by 'clubSandwich':
#>   method    from    
#>   bread.mlm sandwich
#> 
#> Multivariate Meta-Analysis Model (k = 6; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      6     no      id 
#> 
#> Test for Heterogeneity:
#> Q(df = 5) = 4.6339, p-val = 0.4622
#> 
#> Number of estimates:   6
#> Number of clusters:    3
#> Estimates per cluster: 1-3 (mean: 2.00, median: 2)
#> 
#> Model Results:
#> 
#> estimate      se¹     tval¹    df¹    pval¹    ci.lb¹   ci.ub¹    
#>  -0.0479  0.1337   -0.3580   1.66   0.7607   -0.7530   0.6573     
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> 1) results based on cluster-robust inference (var-cov estimator: CR2,
#>    approx t-test and confidence interval, df: Satterthwaite approx)
#> 
```
