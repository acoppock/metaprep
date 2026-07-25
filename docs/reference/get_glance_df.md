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
library(randomizr)
library(estimatr)

set.seed(123)
dat <- data.frame(Z = complete_ra(100, num_arms = 2), Y = rnorm(100))
fit <- lm_robust(Y ~ Z, data = dat)
prepped <- prep_fit(fit, term = "ZT2")
get_glance_df(prepped)
#> # A tibble: 1 × 7
#>   r.squared adj.r.squared statistic p.value df.residual  nobs se_type
#>       <dbl>         <dbl>     <dbl>   <dbl>       <dbl> <int> <chr>  
#> 1  0.000416      -0.00978    0.0408   0.840          98   100 HC2    
```
