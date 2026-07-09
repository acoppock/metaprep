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
rma_mv_helper(object, yi, V = NULL, ...)
```

## Arguments

- object:

  An estimates_vcov object

- yi:

  Formula or bare column name specifying the effect sizes (e.g.,
  `estimate`)

- V:

  Variance-covariance matrix (defaults to the vcov from object)

- ...:

  Additional arguments passed to
  [`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html),
  such as `random`, `mods`, etc.

## Value

An object of class `rma.mv` as returned by
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)

## Examples

``` r
if (requireNamespace("metafor", quietly = TRUE)) {
  set.seed(123)
  dat1 <- data.frame(Y = rnorm(50), Z = sample(c("T0", "T1"), 50, TRUE))
  dat2 <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1", "T2"), 100, TRUE))
  dat3 <- data.frame(Y = rnorm(200), Z = sample(c("T0", "T1", "T2"), 200, TRUE))

  prepped_fits <- dplyr::bind_rows(
    study1 = prep_fit(lm(Y ~ Z, data = dat1), term = "ZT1"),
    study2 = prep_fit(lm(Y ~ Z, data = dat2), term = c("ZT1", "ZT2")),
    study3 = prep_fit(lm(Y ~ Z, data = dat3), term = c("ZT1", "ZT2")),
    .id = "study"
  )
  ev <- as_estimates_vcov(prepped_fits)

  ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
  ev |> rma_mv_helper(yi = estimate, mods = ~ study, random = ~ 1 | id)
  ev |>
    dplyr::filter(study != "study1") |>
    rma_mv_helper(yi = estimate, random = ~ 1 | id)
}
#> 
#> Multivariate Meta-Analysis Model (k = 4; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      4     no      id 
#> 
#> Test for Heterogeneity:
#> Q(df = 3) = 2.4856, p-val = 0.4779
#> 
#> Model Results:
#> 
#> estimate      se    zval    pval    ci.lb   ci.ub    
#>   0.0542  0.1248  0.4339  0.6644  -0.1905  0.2988    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
