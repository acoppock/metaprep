# Combine estimates_vcov objects

Row-binds the estimates of two or more `estimates_vcov` objects and
assembles their variance-covariance matrices into a single
block-diagonal matrix, with zero covariance between objects. Use it when
studies were prepared into separate `estimates_vcov` objects but should
be meta-analyzed together.

This is not a plain row-bind: the block-diagonal vcov is rebuilt so that
it stays synchronized with the stacked estimates, and the `id` column is
renumbered across the combined object.

## Usage

``` r
bind_estimates_vcov(...)
```

## Arguments

- ...:

  Two or more `estimates_vcov` objects, or a single list of them.

## Value

A combined `estimates_vcov` object.

## Examples

``` r
library(dplyr)
library(randomizr)
library(estimatr)

set.seed(123)
dat_a <- data.frame(Z = complete_ra(80, num_arms = 2), Y = rnorm(80))
dat_b <- data.frame(Z = complete_ra(120, num_arms = 3), Y = rnorm(120))

ev_a <- as_estimates_vcov(bind_rows(
  study_1 = prep_fit(lm_robust(Y ~ Z, dat_a), term = "ZT2"),
  .id = "study"
))
ev_b <- as_estimates_vcov(bind_rows(
  study_2 = prep_fit(lm_robust(Y ~ Z, dat_b), term = c("ZT2", "ZT3")),
  .id = "study"
))

# One object, block-diagonal vcov, id renumbered 1..n
bind_estimates_vcov(ev_a, ev_b)
#> <estimates_vcov>
#> # 3 estimates with 3x3 vcov matrix
#> 
#> # A tibble: 3 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     study_1 ZT2    -0.0413     0.196    -0.211   0.833   -0.431     0.348
#> 2 2     study_2 ZT2    -0.307      0.230    -1.34    0.184   -0.762     0.148
#> 3 3     study_2 ZT3    -0.0522     0.215    -0.242   0.809   -0.479     0.374
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
```
