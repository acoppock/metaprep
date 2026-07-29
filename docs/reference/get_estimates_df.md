# Extract estimates from prepped fits or estimates_vcov object

Generic function to extract estimates. Works with both:

- `prepped_fits` tibbles (unnests tidy_obj)

- `estimates_vcov` objects (extracts \$estimates)

## Usage

``` r
get_estimates_df(x, ...)
```

## Arguments

- x:

  Either a prepped_fits tibble or an estimates_vcov object

- ...:

  Additional arguments passed to methods

## Value

A tibble of coefficient estimates

## See also

Other component accessors:
[`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md),
[`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md)

## Examples

``` r
library(randomizr)
library(estimatr)

set.seed(123)
dat <- data.frame(Z = complete_ra(100, num_arms = 2), Y = rnorm(100))
fit <- lm_robust(Y ~ Z, data = dat)
prepped <- prep_fit(fit, term = "ZT2")
get_estimates_df(prepped)
#> # A tibble: 1 × 9
#>   term  estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 ZT2     0.0388     0.192     0.202   0.840   -0.342     0.420    98 Y      

ev <- as_estimates_vcov(prepped)
get_estimates_df(ev)
#> # A tibble: 1 × 10
#>   id    term  estimate std.error statistic p.value conf.low conf.high    df
#>   <chr> <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl>
#> 1 1     ZT2     0.0388     0.192     0.202   0.840   -0.342     0.420    98
#> # ℹ 1 more variable: outcome <chr>
```
