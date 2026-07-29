# Create an estimates_vcov object

Combines tidy coefficient estimates with their corresponding
variance-covariance matrix into a single object that maintains
synchronization through dplyr operations.

This is particularly useful for meta-analysis workflows where you need
to filter, group, or manipulate estimates while keeping the vcov matrix
in sync.

## Usage

``` r
as_estimates_vcov(prepped_fits_df)
```

## Arguments

- prepped_fits_df:

  A tibble created by combining one or more calls to
  [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md).
  Must include list-columns `tidy_obj` and `vcov_obj`.

## Value

An object of class `estimates_vcov` containing:

- estimates:

  A tibble of unnested coefficient estimates with an `id` column

- vcov:

  A block-diagonal variance-covariance matrix with rownames/colnames
  matching `id`

- row_map:

  Integer vector tracking original row indices

## See also

[`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
to build the object from an estimates data frame and a vcov matrix you
already have (e.g. a bootstrapped covariance across experiments that
share subjects), and
[estimates_vcov](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
for what the resulting object guarantees.

Other estimates_vcov objects:
[`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md),
[`dplyr-methods`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md),
[`estimates_vcov`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md),
[`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md),
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(randomizr)
library(estimatr)

set.seed(123)
dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
dat_2 <- data.frame(Z = complete_ra(100, num_arms = 3), Y = rnorm(100))
dat_3 <- data.frame(Z = complete_ra(200, num_arms = 4), Y = rnorm(200))

fit_1 <- lm_robust(Y ~ Z, data = dat_1)
fit_2 <- lm_robust(Y ~ Z, data = dat_2)
fit_3 <- lm_robust(Y ~ Z, data = dat_3)

prepped_fits <- bind_rows(
  study_1 = prep_fit(fit_1, term = "ZT2"),
  study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
  study_3 = prep_fit(fit_3, term = c("ZT2", "ZT3", "ZT4")),
  .id = "study"
)
ev <- as_estimates_vcov(prepped_fits)
ev
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     study_1 ZT2    -0.452      0.276    -1.64    0.108   -1.01      0.103
#> 2 2     study_2 ZT2     0.222      0.274     0.812   0.419   -0.321     0.766
#> 3 3     study_2 ZT3     0.366      0.269     1.36    0.178   -0.169     0.900
#> 4 4     study_3 ZT2    -0.0879     0.197    -0.445   0.656   -0.477     0.301
#> 5 5     study_3 ZT3    -0.0823     0.198    -0.415   0.679   -0.474     0.309
#> 6 6     study_3 ZT4    -0.0556     0.181    -0.306   0.760   -0.413     0.302
#> # ℹ 2 more variables: df <dbl>, outcome <chr>

# dplyr verbs keep the vcov synchronized
ev |> filter(study == "study_2")
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 2     study_2 ZT2      0.222     0.274     0.812   0.419   -0.321     0.766
#> 2 3     study_2 ZT3      0.366     0.269     1.36    0.178   -0.169     0.900
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
ev |> arrange(estimate)
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     study_1 ZT2    -0.452      0.276    -1.64    0.108   -1.01      0.103
#> 2 4     study_3 ZT2    -0.0879     0.197    -0.445   0.656   -0.477     0.301
#> 3 5     study_3 ZT3    -0.0823     0.198    -0.415   0.679   -0.474     0.309
#> 4 6     study_3 ZT4    -0.0556     0.181    -0.306   0.760   -0.413     0.302
#> 5 2     study_2 ZT2     0.222      0.274     0.812   0.419   -0.321     0.766
#> 6 3     study_2 ZT3     0.366      0.269     1.36    0.178   -0.169     0.900
#> # ℹ 2 more variables: df <dbl>, outcome <chr>

# Pass straight to metafor via the helper
ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#> 
#> Multivariate Meta-Analysis Model (k = 6; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      6     no      id 
#> 
#> Test for Heterogeneity:
#> Q(df = 5) = 4.6339, p-val = 0.4622
#> 
#> Model Results:
#> 
#> estimate      se     zval    pval    ci.lb   ci.ub    
#>  -0.0479  0.1189  -0.4028  0.6871  -0.2808  0.1851    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
