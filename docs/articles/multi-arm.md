# Preparing multi-arm trials for meta-analysis

When meta-analyzing effect estimates obtained from multi-arm trials, we
have to account for the dependence across estimates that arises when
multiple treatment arms are compared to the same control group. The
statistical methods for this have been worked out, as has an excellent
software implementation in the `metafor` package. In short, we can pass
a block-diagonal variance-covariance matrix to
[`rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
that appropriately accounts for this dependence.

The annoying part is making the block-diagonal variance-covariance
matrix and keeping it synchronized with the estimates data frame. The
`metaprep` package helps with this bookkeeping by providing an
`estimates_vcov` object that keeps estimates and their
variance-covariance matrix in sync through dplyr operations.

``` r

set.seed(343)
library(dplyr)
library(metaprep)
```

## Three experiments

Let’s make fake data for three experiments. All three have a continuous
outcome. The first is small (N = 50) with two arms. The second is larger
(N = 100) with three arms. The third is the biggest (N = 200) with four
arms.

``` r

make_trial_data <- function(n, n_arms) {
  arms <- paste0("T", seq_len(n_arms))
  data.frame(
    Y = rnorm(n),
    Z = factor(sample(arms, n, replace = TRUE), levels = arms)
  )
}

dat_1 <- make_trial_data(50, 2)
dat_2 <- make_trial_data(100, 3)
dat_3 <- make_trial_data(200, 4)
```

Now we estimate treatment effects:

``` r

fit_1 <- lm(Y ~ Z, data = dat_1)
fit_2 <- lm(Y ~ Z, data = dat_2)
fit_3 <- lm(Y ~ Z, data = dat_3)
```

## Basic workflow: estimates_vcov objects

Here comes the `metaprep` part. We prep each fit, bind them together,
and create an `estimates_vcov` object:

``` r

prepped_fit_1 <- prep_fit(fit_1, term = "ZT2")
prepped_fit_2 <- prep_fit(fit_2, term = c("ZT2", "ZT3"))
prepped_fit_3 <- prep_fit(fit_3, term = c("ZT2", "ZT3", "ZT4"))

fits_df <- bind_rows(
  `Study 1` = prepped_fit_1,
  `Study 2` = prepped_fit_2,
  `Study 3` = prepped_fit_3,
  .id = "study"
)

ev <- as_estimates_vcov(fits_df)
ev
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 7
#>   id    study   term  estimate std.error statistic p.value
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     Study 1 ZT2    -0.153      0.233    -0.654   0.516
#> 2 2     Study 2 ZT2     0.134      0.228     0.588   0.558
#> 3 3     Study 2 ZT3     0.0943     0.225     0.419   0.676
#> 4 4     Study 3 ZT2     0.0737     0.195     0.379   0.705
#> 5 5     Study 3 ZT3     0.0754     0.190     0.398   0.691
#> 6 6     Study 3 ZT4    -0.130      0.191    -0.683   0.496
```

The `estimates_vcov` object keeps the estimates and variance-covariance
matrix synchronized. You can use dplyr verbs on it:

``` r

ev |> filter(study == "Study 2")
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 7
#>   id    study   term  estimate std.error statistic p.value
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 2     Study 2 ZT2     0.134      0.228     0.588   0.558
#> 2 3     Study 2 ZT3     0.0943     0.225     0.419   0.676
ev |> mutate(abs_estimate = abs(estimate))
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 8
#>   id    study   term  estimate std.error statistic p.value abs_estimate
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>        <dbl>
#> 1 1     Study 1 ZT2    -0.153      0.233    -0.654   0.516       0.153 
#> 2 2     Study 2 ZT2     0.134      0.228     0.588   0.558       0.134 
#> 3 3     Study 2 ZT3     0.0943     0.225     0.419   0.676       0.0943
#> 4 4     Study 3 ZT2     0.0737     0.195     0.379   0.705       0.0737
#> 5 5     Study 3 ZT3     0.0754     0.190     0.398   0.691       0.0754
#> 6 6     Study 3 ZT4    -0.130      0.191    -0.683   0.496       0.130
ev |> arrange(desc(estimate))
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 7
#>   id    study   term  estimate std.error statistic p.value
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 2     Study 2 ZT2     0.134      0.228     0.588   0.558
#> 2 3     Study 2 ZT3     0.0943     0.225     0.419   0.676
#> 3 5     Study 3 ZT3     0.0754     0.190     0.398   0.691
#> 4 4     Study 3 ZT2     0.0737     0.195     0.379   0.705
#> 5 6     Study 3 ZT4    -0.130      0.191    -0.683   0.496
#> 6 1     Study 1 ZT2    -0.153      0.233    -0.654   0.516
```

Now we’re ready to do meta-analysis using
[`rma_mv_helper()`](https://acoppock.github.io/metaprep/reference/rma_mv_helper.md):

``` r

library(metafor)
#> Loading required package: Matrix
#> Loading required package: metadat
#> Loading required package: numDeriv
#> 
#> Loading the 'metafor' package (version 5.0-1). For an
#> introduction to the package please type: help(metafor)
library(broom)
meta_fit <- ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
tidy(meta_fit)
#> # A tibble: 1 × 6
#>   term    type    estimate std.error statistic p.value
#>   <chr>   <chr>      <dbl>     <dbl>     <dbl>   <dbl>
#> 1 overall summary  0.00378     0.108    0.0349   0.972
```

The
[`rma_mv_helper()`](https://acoppock.github.io/metaprep/reference/rma_mv_helper.md)
function is a convenience wrapper that automatically passes the
estimates data frame and vcov matrix to
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html).
The `random = ~ 1 | id` argument specifies a random effect for each
estimate (identified by the `id` column that `estimates_vcov` objects
automatically include).

## Alternative workflow: extract components separately

If you prefer to work with the components separately, you can extract
them:

``` r

estimates_df <- get_estimates_df(fits_df)
estimates_df
#> # A tibble: 6 × 6
#>   study   term  estimate std.error statistic p.value
#>   <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 Study 1 ZT2    -0.153      0.233    -0.654   0.516
#> 2 Study 2 ZT2     0.134      0.228     0.588   0.558
#> 3 Study 2 ZT3     0.0943     0.225     0.419   0.676
#> 4 Study 3 ZT2     0.0737     0.195     0.379   0.705
#> 5 Study 3 ZT3     0.0754     0.190     0.398   0.691
#> 6 Study 3 ZT4    -0.130      0.191    -0.683   0.496

vcov_matrix <- get_vcov(fits_df)
vcov_matrix
#> 6 x 6 sparse Matrix of class "dsCMatrix"
#>                                                                       
#> [1,] 0.05435329 .          .          .          .          .         
#> [2,] .          0.05203090 0.02641569 .          .          .         
#> [3,] .          0.02641569 0.05056717 .          .          .         
#> [4,] .          .          .          0.03784292 0.01833626 0.01833626
#> [5,] .          .          .          0.01833626 0.03596728 0.01833626
#> [6,] .          .          .          0.01833626 0.01833626 0.03631299
```

These functions work on both the prepped fits data frame and on
`estimates_vcov` objects:

``` r

estimates_df <- get_estimates_df(ev)
vcov_matrix <- get_vcov(ev)
```

You can also extract model-level summaries with
[`get_glance_df()`](https://acoppock.github.io/metaprep/reference/get_glance_df.md):

``` r

glance_df <- get_glance_df(fits_df)
glance_df
#> # A tibble: 3 × 13
#>   study r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
#>   <chr>     <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 Stud…   0.00884      -0.0118  0.824     0.428   0.516     1  -60.3  127.  132.
#> 2 Stud…   0.00374      -0.0168  0.919     0.182   0.834     2 -132.   272.  282.
#> 3 Stud…   0.00779      -0.00739 0.958     0.513   0.674     3 -273.   556.  573.
#> # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
```

Then conduct meta-analysis the traditional way:

``` r

meta_fit_traditional <- rma.mv(
  yi = estimate, V = vcov_matrix, data = estimates_df, random = ~ 1 | id
)
tidy(meta_fit_traditional)
#> # A tibble: 1 × 6
#>   term    type    estimate std.error statistic p.value
#>   <chr>   <chr>      <dbl>     <dbl>     <dbl>   <dbl>
#> 1 overall summary  0.00378     0.108    0.0349   0.972
```

## Grouping and nesting workflows

The `estimates_vcov` object supports
[`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html) for
grouped meta-analyses:

``` r

ev_grouped <- ev |>
  mutate(arm_type = if_else(term %in% c("ZT2"), "Treatment 2", "Treatment 3or4"))

grouped_results <- ev_grouped |>
  nest_by(arm_type) |>
  mutate(
    meta_fit = list(rma_mv_helper(data, yi = estimate, random = ~ 1 | id)),
    meta_results = list(tidy(meta_fit))
  ) |>
  tidyr::unnest(meta_results)

grouped_results |> select(arm_type, estimate, std.error, p.value)
#> # A tibble: 2 × 4
#> # Groups:   arm_type [2]
#>   arm_type       estimate std.error p.value
#>   <chr>             <dbl>     <dbl>   <dbl>
#> 1 Treatment 2      0.0268     0.125   0.830
#> 2 Treatment 3or4   0.0159     0.133   0.905
```

## What’s going on under the hood

[`prep_fit()`](https://acoppock.github.io/metaprep/reference/prep_fit.md)
conducts three post-estimation operations on the regression fit:
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html), and
[`vcov()`](https://rdrr.io/r/stats/vcov.html). It subsets the rows of
the tidy object and the rows and columns of the vcov object to the terms
relevant for the meta-analysis – in this case excluding the intercept.

The resulting three objects are wrapped in a tibble where the cells are
themselves data frames:

``` r

prepped_fit_1
#> # A tibble: 1 × 3
#>   tidy_obj         glance_obj        vcov_obj     
#>   <list>           <list>            <list>       
#> 1 <tibble [1 × 5]> <tibble [1 × 12]> <dbl [1 × 1]>
```

We then bind all the prepped fits together with an id variable:

``` r

fits_df
#> # A tibble: 3 × 4
#>   study   tidy_obj         glance_obj        vcov_obj     
#>   <chr>   <list>           <list>            <list>       
#> 1 Study 1 <tibble [1 × 5]> <tibble [1 × 12]> <dbl [1 × 1]>
#> 2 Study 2 <tibble [2 × 5]> <tibble [1 × 12]> <dbl [2 × 2]>
#> 3 Study 3 <tibble [3 × 5]> <tibble [1 × 12]> <dbl [3 × 3]>
```

The
[`get_estimates_df()`](https://acoppock.github.io/metaprep/reference/get_estimates_df.md)
function extracts and unnests the estimates:

``` r

get_estimates_df(fits_df)
#> # A tibble: 6 × 6
#>   study   term  estimate std.error statistic p.value
#>   <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 Study 1 ZT2    -0.153      0.233    -0.654   0.516
#> 2 Study 2 ZT2     0.134      0.228     0.588   0.558
#> 3 Study 2 ZT3     0.0943     0.225     0.419   0.676
#> 4 Study 3 ZT2     0.0737     0.195     0.379   0.705
#> 5 Study 3 ZT3     0.0754     0.190     0.398   0.691
#> 6 Study 3 ZT4    -0.130      0.191    -0.683   0.496
```

And the
[`get_vcov()`](https://acoppock.github.io/metaprep/reference/get_vcov.md)
function extracts each `vcov_obj` and combines them into a
block-diagonal matrix:

``` r

get_vcov(fits_df)
#> 6 x 6 sparse Matrix of class "dsCMatrix"
#>                                                                       
#> [1,] 0.05435329 .          .          .          .          .         
#> [2,] .          0.05203090 0.02641569 .          .          .         
#> [3,] .          0.02641569 0.05056717 .          .          .         
#> [4,] .          .          .          0.03784292 0.01833626 0.01833626
#> [5,] .          .          .          0.01833626 0.03596728 0.01833626
#> [6,] .          .          .          0.01833626 0.01833626 0.03631299
```

The
[`as_estimates_vcov()`](https://acoppock.github.io/metaprep/reference/as_estimates_vcov.md)
function creates a special object that keeps these two components
synchronized:

``` r

ev <- as_estimates_vcov(fits_df)
class(ev)
#> [1] "estimates_vcov"
```

This object has an `id` column that links each row of estimates to the
corresponding row/column of the vcov matrix:

``` r

ev$estimates$id
#> [1] "1" "2" "3" "4" "5" "6"
rownames(ev$vcov)
#> [1] "1" "2" "3" "4" "5" "6"
```

When you use dplyr verbs on an `estimates_vcov` object, both the
estimates and vcov are updated together:

``` r

ev_study2 <- ev |> filter(study == "Study 2")
nrow(get_estimates_df(ev_study2))
#> [1] 2
dim(get_vcov(ev_study2))
#> [1] 2 2
```

This synchronization is what makes the `estimates_vcov` object useful
for meta-analysis workflows involving filtering, grouping, and
subsetting.
