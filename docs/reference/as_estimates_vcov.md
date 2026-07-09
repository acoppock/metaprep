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
  [`prep_fit()`](https://acoppock.github.io/metaprep/reference/prep_fit.md).
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

## Examples

``` r
set.seed(123)
dat1 <- data.frame(Y = rnorm(50), Z = sample(c("T0", "T1"), 50, TRUE))
dat2 <- data.frame(Y = rnorm(100), Z = sample(c("T0", "T1", "T2"), 100, TRUE))
dat3 <- data.frame(Y = rnorm(200), Z = sample(c("T0", "T1", "T2"), 200, TRUE))

prepped_fits <- dplyr::bind_rows(
  study1 = prep_fit(lm(Y ~ Z, data = dat1), term = "ZT1"),
  study2 = prep_fit(lm(Y ~ Z, data = dat2), term = c("ZT1", "ZT2")),
  study3 = prep_fit(lm(Y ~ Z, data = dat3), term = c("ZT1", "ZT2")),
  .id = "study"
)
ev <- as_estimates_vcov(prepped_fits)
ev
#> <estimates_vcov>
#> # 5 estimates with 5x5 vcov matrix
#> 
#> # A tibble: 5 × 7
#>   id    study  term  estimate std.error statistic p.value
#>   <chr> <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     study1 ZT1     0.385      0.267     1.44    0.156
#> 2 2     study2 ZT1     0.159      0.239     0.667   0.506
#> 3 3     study2 ZT2    -0.176      0.237    -0.744   0.459
#> 4 4     study3 ZT1     0.0314     0.184     0.170   0.865
#> 5 5     study3 ZT2     0.140      0.174     0.803   0.423

# dplyr verbs keep vcov synchronized
ev |> dplyr::filter(study == "study2")
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 7
#>   id    study  term  estimate std.error statistic p.value
#>   <chr> <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 2     study2 ZT1      0.159     0.239     0.667   0.506
#> 2 3     study2 ZT2     -0.176     0.237    -0.744   0.459
ev |> dplyr::arrange(estimate)
#> <estimates_vcov>
#> # 5 estimates with 5x5 vcov matrix
#> 
#> # A tibble: 5 × 7
#>   id    study  term  estimate std.error statistic p.value
#>   <chr> <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 3     study2 ZT2    -0.176      0.237    -0.744   0.459
#> 2 4     study3 ZT1     0.0314     0.184     0.170   0.865
#> 3 5     study3 ZT2     0.140      0.174     0.803   0.423
#> 4 2     study2 ZT1     0.159      0.239     0.667   0.506
#> 5 1     study1 ZT1     0.385      0.267     1.44    0.156
ev |> dplyr::mutate(se = std.error)
#> <estimates_vcov>
#> # 5 estimates with 5x5 vcov matrix
#> 
#> # A tibble: 5 × 8
#>   id    study  term  estimate std.error statistic p.value    se
#>   <chr> <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl> <dbl>
#> 1 1     study1 ZT1     0.385      0.267     1.44    0.156 0.267
#> 2 2     study2 ZT1     0.159      0.239     0.667   0.506 0.239
#> 3 3     study2 ZT2    -0.176      0.237    -0.744   0.459 0.237
#> 4 4     study3 ZT1     0.0314     0.184     0.170   0.865 0.184
#> 5 5     study3 ZT2     0.140      0.174     0.803   0.423 0.174
```
