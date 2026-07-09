# metaprep

Meta-analyzing effect estimates from multi-arm trials requires
accounting for the dependence that arises when several treatment arms
are compared to the same control group. The methods are well understood
and
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
implements them: you pass a block-diagonal variance-covariance matrix
that encodes the dependence. The annoying part is building that matrix
and keeping it synchronized with the estimates as you filter, group, and
subset.

`metaprep` handles the bookkeeping. It provides an `estimates_vcov`
object that keeps coefficient estimates and their variance-covariance
matrix in sync through dplyr operations, plus thin wrappers around
`metafor` that read from it directly.

## Installation

Install the development version from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("acoppock/metaprep")
```

## Example

Prepare a fit from each trial, bind them together, and build a
synchronized `estimates_vcov` object:

``` r

library(dplyr)
library(metaprep)

set.seed(343)
fit_1 <- lm(Y ~ Z, data = data.frame(Y = rnorm(50),  Z = sample(c("T0", "T1"), 50, TRUE)))
fit_2 <- lm(Y ~ Z, data = data.frame(Y = rnorm(100), Z = sample(c("T0", "T1", "T2"), 100, TRUE)))

fits_df <- bind_rows(
  `Study 1` = prep_fit(fit_1, term = "ZT1"),
  `Study 2` = prep_fit(fit_2, term = c("ZT1", "ZT2")),
  .id = "study"
)

ev <- as_estimates_vcov(fits_df)
ev
#> <estimates_vcov>
#> # 3 estimates with 3x3 vcov matrix
#> 
#> # A tibble: 3 × 7
#>   id    study   term  estimate std.error statistic p.value
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     Study 1 ZT1    -0.153      0.233    -0.654   0.516
#> 2 2     Study 2 ZT1     0.134      0.228     0.588   0.558
#> 3 3     Study 2 ZT2     0.0943     0.225     0.419   0.676
```

dplyr verbs keep the vcov matrix in sync with the estimates:

``` r

ev |> filter(study == "Study 2")
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 7
#>   id    study   term  estimate std.error statistic p.value
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 2     Study 2 ZT1     0.134      0.228     0.588   0.558
#> 2 3     Study 2 ZT2     0.0943     0.225     0.419   0.676
```

Meta-analyze with the `metafor` wrapper, which pulls the estimates and
vcov from the object automatically:

``` r

library(metafor)
#> Loading required package: Matrix
#> Loading required package: metadat
#> Loading required package: numDeriv
#> 
#> Loading the 'metafor' package (version 5.0-1). For an
#> introduction to the package please type: help(metafor)
ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
#> 
#> Multivariate Meta-Analysis Model (k = 3; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      3     no      id 
#> 
#> Test for Heterogeneity:
#> Q(df = 2) = 0.7915, p-val = 0.6732
#> 
#> Model Results:
#> 
#> estimate      se    zval    pval    ci.lb   ci.ub    
#>   0.0027  0.1505  0.0178  0.9858  -0.2923  0.2977    
#> 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

See
[`vignette("metaprep")`](https://alexandercoppock.com/metaprep/articles/metaprep.md)
for the full workflow, including component extraction, grouped
meta-analyses, and what happens under the hood.
