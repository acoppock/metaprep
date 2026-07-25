# Sign-flip or rescale an estimates_vcov object

Multiply each estimate by a per-row factor and update the
variance-covariance matrix to match, keeping the object internally
consistent. Use it to align the sign of estimates across studies (`by`
of `+1` / `-1`) or to change units (e.g. `by = 100` for percentage
points).

This is the correct way to transform estimate *values*. The dplyr
methods keep the vcov row-aligned (subsetting, reordering) but never
transform it, so `mutate(estimate = -estimate)` would flip the estimates
while leaving the vcov (and its cross-study covariances) inconsistent.
`rescale_estimates_vcov()` applies \\V \mapsto \mathrm{diag}(s)\\ V\\
\mathrm{diag}(s)\\, so the covariances stay valid, including the sign of
cross-covariances under a partial sign flip. `std.error`, `statistic`,
and the confidence bounds are updated to match when present.

## Usage

``` r
rescale_estimates_vcov(ev, by)
```

## Arguments

- ev:

  An `estimates_vcov` object.

- by:

  A per-estimate multiplier: a bare column name, an expression evaluated
  in the estimates, or a numeric vector of length 1 (recycled) or
  `nrow(estimates)`. Use `+1` / `-1` to flip signs, positive values to
  rescale.

## Value

An `estimates_vcov` object with `estimate` (and `std.error`,
`statistic`, `conf.low`, `conf.high` when present) and the vcov
rescaled.

## Examples

``` r
library(dplyr)
library(randomizr)
library(estimatr)

set.seed(123)
dat <- data.frame(Z = complete_ra(120, num_arms = 3), Y = rnorm(120))
ev <- as_estimates_vcov(bind_rows(
  study_1 = prep_fit(lm_robust(Y ~ Z, dat), term = c("ZT2", "ZT3")),
  .id = "study"
))

# Flip the sign of the first arm only; the cross-covariance sign updates too
ev |> rescale_estimates_vcov(by = if_else(term == "ZT2", -1, 1))
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     study_1 ZT2     0.0878     0.242     0.363   0.717   -0.391     0.567
#> 2 2     study_1 ZT3    -0.265      0.219    -1.21    0.228   -0.699     0.168
#> # ℹ 2 more variables: df <dbl>, outcome <chr>

# Rescale to percentage points
ev |> rescale_estimates_vcov(by = 100)
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     study_1 ZT2      -8.78      24.2    -0.363   0.717    -56.7      39.1
#> 2 2     study_1 ZT3     -26.5       21.9    -1.21    0.228    -69.9      16.8
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
```
