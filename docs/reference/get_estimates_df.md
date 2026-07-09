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

## Examples

``` r
set.seed(123)
dat <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1"), 100, TRUE))
prepped <- prep_fit(lm(Y ~ Z, data = dat), term = "ZT1")
get_estimates_df(prepped)
#> # A tibble: 1 × 5
#>   term  estimate std.error statistic p.value
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 ZT1      0.282     0.181      1.56   0.122

ev <- as_estimates_vcov(prepped)
get_estimates_df(ev)
#> # A tibble: 1 × 6
#>   id    term  estimate std.error statistic p.value
#>   <chr> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     ZT1      0.282     0.181      1.56   0.122
```
