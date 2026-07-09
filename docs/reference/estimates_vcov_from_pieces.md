# Create estimates_vcov from separate estimates and vcov

Alternative constructor for estimates_vcov objects when you already have
the estimates data frame and block-diagonal vcov matrix separately.

This is useful if you've already called
[`get_estimates_df()`](https://acoppock.github.io/metaprep/reference/get_estimates_df.md)
and
[`get_vcov()`](https://acoppock.github.io/metaprep/reference/get_vcov.md)
and want to combine them into a synchronized object.

## Usage

``` r
estimates_vcov_from_pieces(estimates_df, vcov_matrix)
```

## Arguments

- estimates_df:

  A data frame or tibble of coefficient estimates, typically from
  [`get_estimates_df()`](https://acoppock.github.io/metaprep/reference/get_estimates_df.md).

- vcov_matrix:

  A variance-covariance matrix, typically from
  [`get_vcov()`](https://acoppock.github.io/metaprep/reference/get_vcov.md).
  Must have dimensions matching the number of rows in estimates_df.

## Value

An object of class `estimates_vcov`

## Examples

``` r
set.seed(123)
dat <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1"), 100, TRUE))
prepped <- prep_fit(lm(Y ~ Z, data = dat), term = "ZT1")
estimates_df <- get_estimates_df(prepped)
vcov_matrix <- as.matrix(get_vcov(prepped))
estimates_vcov_from_pieces(estimates_df, vcov_matrix)
#> <estimates_vcov>
#> # 1 estimates with 1x1 vcov matrix
#> 
#> # A tibble: 1 × 6
#>   id    term  estimate std.error statistic p.value
#>   <chr> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     ZT1      0.282     0.181      1.56   0.122
```
