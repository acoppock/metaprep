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

## Examples

``` r
set.seed(123)
dat1 <- data.frame(Y = rnorm(50), Z = sample(c("T0", "T1"), 50, TRUE))
dat2 <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1", "T2"), 100, TRUE))

prepped_fits <- dplyr::bind_rows(
  study1 = prep_fit(lm(Y ~ Z, data = dat1), term = "ZT1"),
  study2 = prep_fit(lm(Y ~ Z, data = dat2), term = c("ZT1", "ZT2")),
  .id = "study"
)
vcov_matrix <- get_vcov(prepped_fits)
dim(vcov_matrix)
#> [1] 3 3

ev <- as_estimates_vcov(prepped_fits)
get_vcov(ev)
#>            1          2          3
#> 1 0.07120511 0.00000000 0.00000000
#> 2 0.00000000 0.05711795 0.02728021
#> 3 0.00000000 0.02728021 0.05621377
```
