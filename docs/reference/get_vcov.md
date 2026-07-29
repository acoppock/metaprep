# Extract variance-covariance matrix from prepped fits or estimates_vcov object

Generic function to extract vcov matrix. Works with both:

- `prepped_fits` tibbles (creates block-diagonal matrix)

- `estimates_vcov` objects (extracts \$vcov)

## Usage

``` r
get_vcov(x, ...)
```

## Arguments

- x:

  Either a prepped_fits tibble or an estimates_vcov object

- ...:

  Additional arguments passed to methods

## Value

A variance-covariance matrix (block-diagonal for prepped_fits)

## See also

Other component accessors:
[`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md),
[`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md)

## Examples

``` r
library(dplyr)
library(randomizr)
library(estimatr)

set.seed(123)
dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
dat_2 <- data.frame(Z = complete_ra(100, num_arms = 3), Y = rnorm(100))

fit_1 <- lm_robust(Y ~ Z, data = dat_1)
fit_2 <- lm_robust(Y ~ Z, data = dat_2)

prepped_fits <- bind_rows(
  study_1 = prep_fit(fit_1, term = "ZT2"),
  study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
  .id = "study"
)
# Block-diagonal vcov across studies, ready for metafor
get_vcov(prepped_fits)
#> 3 x 3 sparse Matrix of class "dsCMatrix"
#>                                     
#> [1,] 0.0762514 .          .         
#> [2,] .         0.07500602 0.03603387
#> [3,] .         0.03603387 0.07251291

ev <- as_estimates_vcov(prepped_fits)
ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#> 
#> Multivariate Meta-Analysis Model (k = 3; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0737  0.2716      3     no      id 
#> 
#> Test for Heterogeneity:
#> Q(df = 2) = 4.5405, p-val = 0.1033
#> 
#> Model Results:
#> 
#> estimate      se    zval    pval    ci.lb   ci.ub    
#>   0.0114  0.2386  0.0478  0.9619  -0.4563  0.4791    
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
