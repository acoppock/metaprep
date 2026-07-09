# Prepare a Model Fit Object for Tidy Extraction

Extracts selected term-level information from a fitted model object,
returning a tibble with list-columns containing the tidied coefficients,
model summary, and corresponding variance-covariance matrix subset.

This function works with any model that has `tidy()`, `glance()`, and
[`vcov()`](https://rdrr.io/r/stats/vcov.html) methods defined. For
multivariate models (multiple outcomes), the function will attempt to
construct appropriate term names by combining outcome and term names.

## Usage

``` r
prep_fit(fit, term, match = c("exact", "regex"), handle_multivariate = TRUE)
```

## Arguments

- fit:

  A fitted model object with `tidy()`, `glance()`, and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) methods.

- term:

  A character vector of term names or regex patterns to match within the
  model coefficients (e.g., `c("ZT1", "ZT2")`).

- match:

  How to match `term` against coefficient names. `"exact"` (default)
  requires the term to match a coefficient name exactly. `"regex"` uses
  each element of `term` as a regular expression (the elements are
  collapsed with `|`).

- handle_multivariate:

  Logical. If `TRUE` (default), attempts to detect and handle
  multivariate models by creating term names in the format
  "outcome:term". Set to `FALSE` if you want to use the term names as-is
  from `tidy()`.

## Value

A tibble with one row and the following list-columns:

- tidy_obj:

  A tibble of tidied coefficient estimates for the selected terms.

- glance_obj:

  A tibble of model-level summary statistics (from
  [`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html)).

- vcov_obj:

  A numeric matrix of the variance-covariance subset corresponding to
  the selected terms.

## Examples

``` r
set.seed(123)
dat <- data.frame(
  Y = rnorm(200),
  Z = sample(c("T0", "T1", "T2"), 200, replace = TRUE)
)
fit <- lm(Y ~ Z, data = dat)

# Extract two treatment arms
prep_fit(fit, term = c("ZT1", "ZT2"))
#> # A tibble: 1 × 3
#>   tidy_obj         glance_obj        vcov_obj     
#>   <list>           <list>            <list>       
#> 1 <tibble [2 × 5]> <tibble [1 × 12]> <dbl [2 × 2]>

# Regex matching captures all ZT-prefixed terms at once
prep_fit(fit, term = "ZT", match = "regex")
#> # A tibble: 1 × 3
#>   tidy_obj         glance_obj        vcov_obj     
#>   <list>           <list>            <list>       
#> 1 <tibble [2 × 5]> <tibble [1 × 12]> <dbl [2 × 2]>

# Combine multiple studies and create an estimates_vcov object
dat2 <- data.frame(Y = rnorm(150), Z = sample(c("T0", "T1", "T2"), 150, TRUE))
prepped_fits <- dplyr::bind_rows(
  study1 = prep_fit(lm(Y ~ Z, data = dat), term = c("ZT1", "ZT2")),
  study2 = prep_fit(lm(Y ~ Z, data = dat2), term = c("ZT1", "ZT2")),
  .id = "study"
)
as_estimates_vcov(prepped_fits)
#> <estimates_vcov>
#> # 4 estimates with 4x4 vcov matrix
#> 
#> # A tibble: 4 × 7
#>   id    study  term  estimate std.error statistic p.value
#>   <chr> <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     study1 ZT1    -0.0227     0.170    -0.134   0.894
#> 2 2     study1 ZT2     0.104      0.168     0.618   0.537
#> 3 3     study2 ZT1     0.0564     0.213     0.265   0.792
#> 4 4     study2 ZT2     0.150      0.210     0.713   0.477
```
