# Run rma.uni on an estimates_vcov object

A convenience wrapper for
[`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
that automatically extracts the estimates data frame and variance
estimates from an estimates_vcov object.

Note: This function uses the diagonal of the vcov matrix as the variance
estimates. If you have correlated estimates (e.g., from multi-arm
trials), use
[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
instead to properly account for the correlation structure.

## Usage

``` r
rma_uni_helper(object, yi, vi = NULL, cluster = NULL, clubSandwich = TRUE, ...)
```

## Arguments

- object:

  An estimates_vcov object

- yi:

  Formula or bare column name specifying the effect sizes (e.g.,
  `estimate`)

- vi:

  Numeric vector specifying the variances (defaults to diag(vcov))

- cluster:

  Optional bare column name (evaluated in the estimates data frame, like
  `yi`) giving the clustering variable for cluster-robust (sandwich)
  standard errors via
  [`metafor::robust()`](https://wviechtb.github.io/metafor/reference/robust.html).
  `NULL` (default) returns the ordinary model-based fit.

- clubSandwich:

  Logical, passed to
  [`metafor::robust()`](https://wviechtb.github.io/metafor/reference/robust.html)
  when `cluster` is supplied. `TRUE` (default) requests CR2 standard
  errors via the clubSandwich package; `FALSE` uses metafor's CR0
  estimator.

- ...:

  Additional arguments passed to
  [`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)

## Value

An object of class `rma.uni` as returned by
[`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html),
or, when `cluster` is supplied, a `robust.rma` object from
[`metafor::robust()`](https://wviechtb.github.io/metafor/reference/robust.html).

## Examples

``` r
library(dplyr)
library(randomizr)
library(estimatr)

set.seed(123)
dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
dat_2 <- data.frame(Z = complete_ra(100, num_arms = 2), Y = rnorm(100))
dat_3 <- data.frame(Z = complete_ra(200, num_arms = 2), Y = rnorm(200))

fit_1 <- lm_robust(Y ~ Z, data = dat_1)
fit_2 <- lm_robust(Y ~ Z, data = dat_2)
fit_3 <- lm_robust(Y ~ Z, data = dat_3)

prepped_fits <- bind_rows(
  study_1 = prep_fit(fit_1, term = "ZT2"),
  study_2 = prep_fit(fit_2, term = "ZT2"),
  study_3 = prep_fit(fit_3, term = "ZT2"),
  .id = "study"
)
ev <- as_estimates_vcov(prepped_fits)

ev |> rma_uni_helper(yi = estimate)
#> 
#> Random-Effects Model (k = 3; tau^2 estimator: REML)
#> 
#> tau^2 (estimated amount of total heterogeneity): 0.0208 (SE = 0.0626)
#> tau (square root of estimated tau^2 value):      0.1443
#> I^2 (total heterogeneity / total variability):   33.22%
#> H^2 (total variability / sampling variability):  1.50
#> 
#> Test for Heterogeneity:
#> Q(df = 2) = 3.4840, p-val = 0.1752
#> 
#> Model Results:
#> 
#> estimate      se     zval    pval    ci.lb   ci.ub    
#>  -0.0550  0.1408  -0.3910  0.6958  -0.3310  0.2209    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
ev |>
  mutate(large_study = study == "study_3") |>
  rma_uni_helper(yi = estimate, mods = ~ large_study)
#> 
#> Mixed-Effects Model (k = 3; tau^2 estimator: REML)
#> 
#> tau^2 (estimated amount of residual heterogeneity):     0.1561 (SE = 0.3097)
#> tau (square root of estimated tau^2 value):             0.3951
#> I^2 (residual heterogeneity / unaccounted variability): 71.29%
#> H^2 (unaccounted variability / sampling variability):   3.48
#> R^2 (amount of heterogeneity accounted for):            0.00%
#> 
#> Test for Residual Heterogeneity:
#> QE(df = 1) = 3.4827, p-val = 0.0620
#> 
#> Test of Moderators (coefficient 2):
#> QM(df = 1) = 0.0119, p-val = 0.9132
#> 
#> Model Results:
#> 
#>                  estimate      se     zval    pval    ci.lb   ci.ub    
#> intrcpt           -0.1007  0.3303  -0.3050  0.7604  -0.7481  0.5466    
#> large_studyTRUE    0.0580  0.5319   0.1091  0.9132  -0.9846  1.1006    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
