# Run rma.uni on an estimates_vcov object

A convenience wrapper for
[`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
that automatically extracts the estimates data frame and variance
estimates from an estimates_vcov object.

Note: This function uses the diagonal of the vcov matrix as the variance
estimates. If you have correlated estimates (e.g., from multi-arm
trials), use
[`rma_mv_helper()`](https://acoppock.github.io/metaprep/reference/rma_mv_helper.md)
instead to properly account for the correlation structure.

## Usage

``` r
rma_uni_helper(object, yi, vi = NULL, ...)
```

## Arguments

- object:

  An estimates_vcov object

- yi:

  Formula or bare column name specifying the effect sizes (e.g.,
  `estimate`)

- vi:

  Numeric vector specifying the variances (defaults to diag(vcov))

- ...:

  Additional arguments passed to
  [`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)

## Value

An object of class `rma.uni` as returned by
[`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)

## Examples

``` r
if (requireNamespace("metafor", quietly = TRUE)) {
  set.seed(123)
  dat1 <- data.frame(Y = rnorm(50), Z = sample(c("T0", "T1"), 50, TRUE))
  dat2 <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1"), 100, TRUE))
  dat3 <- data.frame(Y = rnorm(200), Z = sample(c("T0", "T1"), 200, TRUE))

  prepped_fits <- dplyr::bind_rows(
    study1 = prep_fit(lm(Y ~ Z, data = dat1), term = "ZT1"),
    study2 = prep_fit(lm(Y ~ Z, data = dat2), term = "ZT1"),
    study3 = prep_fit(lm(Y ~ Z, data = dat3), term = "ZT1"),
    .id = "study"
  )
  ev <- as_estimates_vcov(prepped_fits)

  ev |> rma_uni_helper(yi = estimate)
  ev |>
    dplyr::mutate(large_study = study == "study3") |>
    rma_uni_helper(yi = estimate, mods = ~ large_study)
}
#> 
#> Mixed-Effects Model (k = 3; tau^2 estimator: REML)
#> 
#> tau^2 (estimated amount of residual heterogeneity):     0 (SE = 0.0778)
#> tau (square root of estimated tau^2 value):             0
#> I^2 (residual heterogeneity / unaccounted variability): 0.00%
#> H^2 (unaccounted variability / sampling variability):   1.00
#> R^2 (amount of heterogeneity accounted for):            0.00%
#> 
#> Test for Residual Heterogeneity:
#> QE(df = 1) = 0.4489, p-val = 0.5029
#> 
#> Test of Moderators (coefficient 2):
#> QM(df = 1) = 0.9489, p-val = 0.3300
#> 
#> Model Results:
#> 
#>                  estimate      se     zval    pval    ci.lb   ci.ub    
#> intrcpt            0.2408  0.1585   1.5187  0.1288  -0.0700  0.5515    
#> large_studyTRUE   -0.2062  0.2117  -0.9741  0.3300  -0.6211  0.2087    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
