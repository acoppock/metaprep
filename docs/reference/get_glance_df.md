# Extract glance summary from prepped fits or estimates_vcov object

Generic function to extract model-level summaries. Works with:

- `prepped_fits` tibbles (unnests glance_obj)

- `estimates_vcov` objects (not applicable - returns NULL with warning)

## Usage

``` r
get_glance_df(x, ...)
```

## Arguments

- x:

  Either a prepped_fits tibble or an estimates_vcov object

- ...:

  Additional arguments passed to methods

## Value

A tibble of model-level statistics (or NULL for estimates_vcov)

## Examples

``` r
set.seed(123)
dat <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1"), 100, TRUE))
prepped <- prep_fit(lm(Y ~ Z, data = dat), term = "ZT1")
get_glance_df(prepped)
#> # A tibble: 1 × 12
#>   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
#>       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1    0.0242        0.0142 0.906      2.43   0.122     1  -131.  268.  276.
#> # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
```
