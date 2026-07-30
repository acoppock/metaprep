# Create an estimates_vcov from estimates and a vcov you already have

Constructor for `estimates_vcov` objects from an estimates data frame
and a variance-covariance matrix supplied directly, rather than read off
fitted models by
[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md).

Use it whenever the covariances do not come out of a single regression.
The main case is estimates that are correlated because they share
subjects but cannot be stacked into one model: several experiments run
on overlapping samples, where the covariance between their estimates is
obtained by bootstrapping the whole design and taking
[`cov()`](https://rdrr.io/r/stats/cor.html) of the replicate estimates.
It also serves the plumbing case of recombining the output of
[`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md)
and
[`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md).

The vcov is matched to the estimates **by position**: row `i` of
`estimates_df` is row and column `i` of `vcov_matrix`. Any dimnames on
`vcov_matrix` are discarded and replaced with the object's `id`, so
build both from the same ordered set of terms.

## Usage

``` r
make_estimates_vcov(estimates_df, vcov_matrix)
```

## Arguments

- estimates_df:

  A data frame or tibble of coefficient estimates. Must contain an
  `estimate` column to be usable downstream; a `std.error` column
  (typically `sqrt(diag(vcov_matrix))`) is recommended so that
  [`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
  has standard errors to rescale.

- vcov_matrix:

  A variance-covariance matrix, in the same row order as `estimates_df`.
  Must be square, symmetric, and of the same dimension as
  `nrow(estimates_df)`. A base matrix or a `Matrix` object are both
  accepted and neither is converted, so the storage of the result
  follows what you supply.

## Value

An object of class `estimates_vcov`

## See also

[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
to build the object from
[`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
output, and
[`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md)
to combine the result with other objects.

Other estimates_vcov objects:
[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md),
[`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md),
[`dplyr-methods`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md),
[`estimates_vcov`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md),
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)

## Examples

``` r
# Two experiments on overlapping subjects: every subject takes the survey
# experiment, a random third also takes the lab experiment. The two effect
# estimates are correlated, but there is no single regression to read the
# covariance off, so bootstrap the design and use cov() of the replicates.
set.seed(123)
n <- 400
dat <- data.frame(
  Z_survey = rbinom(n, 1, 0.5),
  in_lab = rbinom(n, 1, 1 / 3)
)
dat$Z_lab <- ifelse(dat$in_lab == 1, rbinom(n, 1, 0.5), NA)
dat$Y_survey <- 0.2 * dat$Z_survey + rnorm(n)
dat$Y_lab <- 0.5 * dat$Z_lab + 0.6 * dat$Y_survey + rnorm(n)

estimate_both <- function(d) {
  c(
    survey = coef(lm(Y_survey ~ Z_survey, data = d))[["Z_survey"]],
    lab = coef(lm(Y_lab ~ Z_lab, data = d[d$in_lab == 1, ]))[["Z_lab"]]
  )
}

# Resample subjects, not rows within experiment, so the shared-sample
# correlation is what the replicates reproduce
boots <- t(replicate(200, estimate_both(dat[sample(n, n, replace = TRUE), ])))
V <- cov(boots)
point <- estimate_both(dat)

estimates_df <- data.frame(
  study = "study_1",
  term = names(point),
  estimate = point,
  std.error = sqrt(diag(V))
)

ev <- make_estimates_vcov(estimates_df, V)
ev
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 5
#>   id    study   term   estimate std.error
#>   <chr> <chr>   <chr>     <dbl>     <dbl>
#> 1 1     study_1 survey    0.385     0.106
#> 2 2     study_1 lab       0.720     0.228
get_vcov(ev)
#>             1           2
#> 1 0.011197075 0.003696527
#> 2 0.003696527 0.052076563
```
