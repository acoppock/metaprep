# Prepare a Model Fit Object for Tidy Extraction

Extracts selected term-level information from a fitted model object,
returning a tibble with list-columns containing the tidied coefficients,
model summary, and corresponding variance-covariance matrix subset.

This function works with any model that has
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html), and
[`vcov()`](https://rdrr.io/r/stats/vcov.html) methods defined. For
multivariate models (multiple outcomes), the function will attempt to
construct appropriate term names by combining outcome and term names.

## Usage

``` r
prep_fit(fit, term, match = c("exact", "regex"), handle_multivariate = TRUE)
```

## Arguments

- fit:

  A fitted model object with
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html), and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) methods.

- term:

  Terms to keep from the model. Either a character vector of exact term
  names (or regex patterns when `match = "regex"`), or a tidyselect
  expression resolved against the model's term names, e.g.
  `starts_with("Z")`, `matches("^Z_treated$")`, or
  `starts_with("Z_treated") & !contains(":")` to take a treatment's main
  effect while dropping its interaction terms.

- match:

  How to match `term` against coefficient names. `"exact"` (default)
  requires the term to match a coefficient name exactly. `"regex"` uses
  each element of `term` as a regular expression (the elements are
  collapsed with `|`).

- handle_multivariate:

  Logical. If `TRUE` (default), attempts to detect and handle
  multivariate models by creating term names in the format
  "outcome:term". Set to `FALSE` if you want to use the term names as-is
  from [`tidy()`](https://generics.r-lib.org/reference/tidy.html).

## Value

A tibble with one row and the following list-columns:

- tidy_obj:

  A tibble of tidied coefficient estimates for the selected terms.

- glance_obj:

  A tibble of model-level summary statistics (from
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)).

- vcov_obj:

  A numeric matrix of the variance-covariance subset corresponding to
  the selected terms.

## Examples

``` r
library(dplyr)
library(randomizr)
library(estimatr)

set.seed(123)
dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
dat_2 <- data.frame(Z = complete_ra(100, num_arms = 3), Y = rnorm(100))
dat_3 <- data.frame(Z = complete_ra(200, num_arms = 4), Y = rnorm(200))

fit_1 <- lm_robust(Y ~ Z, data = dat_1)
fit_2 <- lm_robust(Y ~ Z, data = dat_2)
fit_3 <- lm_robust(Y ~ Z, data = dat_3)

# Extract the treatment arms from a fit
prep_fit(fit_1, term = "ZT2")
#> # A tibble: 1 × 3
#>   tidy_obj         glance_obj   vcov_obj     
#>   <list>           <list>       <list>       
#> 1 <tibble [1 × 9]> <df [1 × 7]> <dbl [1 × 1]>

# Regex matching captures all ZT-prefixed terms at once
prep_fit(fit_3, term = "ZT", match = "regex")
#> # A tibble: 1 × 3
#>   tidy_obj         glance_obj   vcov_obj     
#>   <list>           <list>       <list>       
#> 1 <tibble [3 × 9]> <df [1 × 7]> <dbl [3 × 3]>

# Or select terms with tidyselect (no hand-built coefficient-name vector)
prep_fit(fit_3, starts_with("Z"))
#> # A tibble: 1 × 3
#>   tidy_obj         glance_obj   vcov_obj     
#>   <list>           <list>       <list>       
#> 1 <tibble [3 × 9]> <df [1 × 7]> <dbl [3 × 3]>

# Combine studies, build an estimates_vcov object, and meta-analyze
prepped_fits <- bind_rows(
  study_1 = prep_fit(fit_1, term = "ZT2"),
  study_2 = prep_fit(fit_2, term = c("ZT2", "ZT3")),
  study_3 = prep_fit(fit_3, term = c("ZT2", "ZT3", "ZT4")),
  .id = "study"
)
ev <- as_estimates_vcov(prepped_fits)
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
