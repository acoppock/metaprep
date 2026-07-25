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
library(randomizr)
library(estimatr)
library(metaprep)
```

## Three experiments

Let’s make fake data for three experiments. All three have a continuous
outcome. The first is small (N = 50) with two arms. The second is larger
(N = 100) with three arms. The third is the biggest (N = 200) with four
arms. We use
[`randomizr::complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.html)
to assign the treatment arm `Z`, which performs complete random
assignment and automatically labels the arms `T1`, `T2`, and so on.

``` r

dat_1 <- data.frame(Z = complete_ra(50, num_arms = 2), Y = rnorm(50))
dat_2 <- data.frame(Z = complete_ra(100, num_arms = 3), Y = rnorm(100))
dat_3 <- data.frame(Z = complete_ra(200, num_arms = 4), Y = rnorm(200))
```

Now we estimate treatment effects with
[`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html),
which returns heteroskedasticity-robust standard errors:

``` r

fit_1 <- lm_robust(Y ~ Z, data = dat_1)
fit_2 <- lm_robust(Y ~ Z, data = dat_2)
fit_3 <- lm_robust(Y ~ Z, data = dat_3)
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
#> # A tibble: 6 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     Study 1 ZT2    0.476       0.294    1.62     0.112   -0.115     1.07 
#> 2 2     Study 2 ZT2    0.00575     0.277    0.0208   0.983   -0.544     0.555
#> 3 3     Study 2 ZT3    0.135       0.243    0.556    0.580   -0.347     0.617
#> 4 4     Study 3 ZT2    0.0422      0.189    0.223    0.824   -0.331     0.416
#> 5 5     Study 3 ZT3    0.145       0.206    0.706    0.481   -0.260     0.551
#> 6 6     Study 3 ZT4   -0.0424      0.179   -0.237    0.813   -0.395     0.311
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
```

The `estimates_vcov` object keeps the estimates and variance-covariance
matrix synchronized. You can use dplyr verbs on it:

``` r

ev |> filter(study == "Study 2")
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 2     Study 2 ZT2    0.00575     0.277    0.0208   0.983   -0.544     0.555
#> 2 3     Study 2 ZT3    0.135       0.243    0.556    0.580   -0.347     0.617
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
ev |> mutate(abs_estimate = abs(estimate))
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 12
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     Study 1 ZT2    0.476       0.294    1.62     0.112   -0.115     1.07 
#> 2 2     Study 2 ZT2    0.00575     0.277    0.0208   0.983   -0.544     0.555
#> 3 3     Study 2 ZT3    0.135       0.243    0.556    0.580   -0.347     0.617
#> 4 4     Study 3 ZT2    0.0422      0.189    0.223    0.824   -0.331     0.416
#> 5 5     Study 3 ZT3    0.145       0.206    0.706    0.481   -0.260     0.551
#> 6 6     Study 3 ZT4   -0.0424      0.179   -0.237    0.813   -0.395     0.311
#> # ℹ 3 more variables: df <dbl>, outcome <chr>, abs_estimate <dbl>
ev |> arrange(desc(estimate))
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     Study 1 ZT2    0.476       0.294    1.62     0.112   -0.115     1.07 
#> 2 5     Study 3 ZT3    0.145       0.206    0.706    0.481   -0.260     0.551
#> 3 3     Study 2 ZT3    0.135       0.243    0.556    0.580   -0.347     0.617
#> 4 4     Study 3 ZT2    0.0422      0.189    0.223    0.824   -0.331     0.416
#> 5 2     Study 2 ZT2    0.00575     0.277    0.0208   0.983   -0.544     0.555
#> 6 6     Study 3 ZT4   -0.0424      0.179   -0.237    0.813   -0.395     0.311
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
```

Now we’re ready to do meta-analysis using
[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md):

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
#> 1 overall summary    0.117     0.116      1.01   0.314
```

The
[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
function is a convenience wrapper that automatically passes the
estimates data frame and vcov matrix to
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html).
The `random = ~ 1 | id` argument specifies a random effect for each
estimate (identified by the `id` column that `estimates_vcov` objects
automatically include).

## Cluster-robust standard errors

Meta-analyses of multi-arm trials usually want standard errors clustered
by study, since arms from the same study share variation the
block-diagonal vcov does not capture. Pass a `cluster` column and
[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
wraps the fit in
[`metafor::robust()`](https://wviechtb.github.io/metafor/reference/robust.html)
for you, so you get cluster-robust standard errors in one step:

``` r

ev |>
  rma_mv_helper(yi = estimate, random = ~ 1 | id, cluster = study) |>
  tidy(conf.int = TRUE)
#> # A tibble: 1 × 8
#>   term    type    estimate std.error statistic p.value conf.low conf.high
#>   <chr>   <chr>      <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 overall summary    0.117    0.0961      1.22   0.374   -0.424     0.658
```

`cluster = study` is evaluated in the estimates, exactly like `yi`. By
default `clubSandwich = TRUE` requests CR2 standard errors; set it to
`FALSE` for metafor’s CR0 estimator.

## Meta-regression with moderators

To ask whether effects vary with a study-level or arm-level
characteristic, add the moderator to the object with
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and pass
it through `mods`. Keeping the moderator on the `estimates_vcov` object,
rather than on a detached data frame, is what keeps it aligned with the
vcov:

``` r

ev |>
  mutate(big_study = study == "Study 3") |>
  rma_mv_helper(yi = estimate, mods = ~ big_study, random = ~ 1 | id) |>
  tidy(conf.int = TRUE)
#> # A tibble: 2 × 8
#>   term          type    estimate std.error statistic p.value conf.low conf.high
#>   <chr>         <chr>      <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 intercept     summary    0.237     0.181     1.31    0.190   -0.117     0.591
#> 2 big_studyTRUE summary   -0.204     0.236    -0.865   0.387   -0.667     0.259
```

If a `mods` formula names a column that is not on the object,
[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
stops with an error instead of silently dropping the moderator, so a
variable that never made it onto the object cannot quietly produce an
empty regression.

### Reading the pooled fit

[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
returns a standard `metafor` object (or a `robust.rma` object when
`cluster` is supplied), so
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
read it directly, with no need to pull slots out by hand:

``` r

glance(meta_fit)
#> # A tibble: 1 × 12
#>   tau.squared cochran.qe p.value.cochran.qe cochran.qm p.value.cochran.qm
#>         <dbl>      <dbl>              <dbl>      <dbl>              <dbl>
#> 1           0       2.98              0.703       1.01              0.314
#> # ℹ 7 more variables: df.residual <int>, logLik <dbl>, deviance <dbl>,
#> #   AIC <dbl>, BIC <dbl>, AICc <dbl>, nobs <int>
```

[`glance()`](https://generics.r-lib.org/reference/glance.html) gives the
scalar model summary and, for `rma.uni` or structured `rma.mv` fits, the
heterogeneity `tau^2`. For a multilevel `rma.mv` (for example
`random = list(~ 1 | study, ~ 1 | id)`) the variance components live in
`meta_fit$sigma2`, which
[`glance()`](https://generics.r-lib.org/reference/glance.html) does not
expand. That matches broom’s convention for mixed models, where
per-group variances are not folded into the one-row
[`glance()`](https://generics.r-lib.org/reference/glance.html) summary.

## Alternative workflow: extract components separately

If you prefer to work with the components separately, you can extract
them:

``` r

estimates_df <- get_estimates_df(fits_df)
estimates_df
#> # A tibble: 6 × 10
#>   study   term  estimate std.error statistic p.value conf.low conf.high    df
#>   <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl>
#> 1 Study 1 ZT2    0.476       0.294    1.62     0.112   -0.115     1.07     48
#> 2 Study 2 ZT2    0.00575     0.277    0.0208   0.983   -0.544     0.555    97
#> 3 Study 2 ZT3    0.135       0.243    0.556    0.580   -0.347     0.617    97
#> 4 Study 3 ZT2    0.0422      0.189    0.223    0.824   -0.331     0.416   196
#> 5 Study 3 ZT3    0.145       0.206    0.706    0.481   -0.260     0.551   196
#> 6 Study 3 ZT4   -0.0424      0.179   -0.237    0.813   -0.395     0.311   196
#> # ℹ 1 more variable: outcome <chr>

vcov_matrix <- get_vcov(fits_df)
vcov_matrix
#> 6 x 6 sparse Matrix of class "dsCMatrix"
#>                                                                       
#> [1,] 0.08623141 .          .          .          .          .         
#> [2,] .          0.07670518 0.04029901 .          .          .         
#> [3,] .          0.04029901 0.05898700 .          .          .         
#> [4,] .          .          .          0.03584081 0.01672818 0.01672818
#> [5,] .          .          .          0.01672818 0.04231300 0.01672818
#> [6,] .          .          .          0.01672818 0.01672818 0.03201820
```

These functions work on both the prepped fits data frame and on
`estimates_vcov` objects:

``` r

estimates_df <- get_estimates_df(ev)
vcov_matrix <- get_vcov(ev)
```

You can also extract model-level summaries with
[`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md):

``` r

glance_df <- get_glance_df(fits_df)
glance_df
#> # A tibble: 3 × 8
#>   study   r.squared adj.r.squared statistic p.value df.residual  nobs se_type
#>   <chr>       <dbl>         <dbl>     <dbl>   <dbl>       <dbl> <int> <chr>  
#> 1 Study 1   0.0518         0.0320     2.62    0.112          48    50 HC2    
#> 2 Study 2   0.00383       -0.0167     0.231   0.795          97   100 HC2    
#> 3 Study 3   0.00513       -0.0101     0.305   0.822         196   200 HC2
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
#> 1 overall summary    0.117     0.116      1.01   0.314
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
#> 1 Treatment 2      0.129      0.138   0.351
#> 2 Treatment 3or4   0.0609     0.135   0.652
```

## Combining and transforming objects

If you built several `estimates_vcov` objects separately, for example
one per data source,
[`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md)
stacks them into one, assembling a block-diagonal vcov with zero
covariance between objects:

``` r

ev_a <- as_estimates_vcov(bind_rows(`Study 1` = prepped_fit_1, .id = "study"))
ev_b <- as_estimates_vcov(bind_rows(`Study 2` = prepped_fit_2, `Study 3` = prepped_fit_3, .id = "study"))
bind_estimates_vcov(ev_a, ev_b)
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     Study 1 ZT2    0.476       0.294    1.62     0.112   -0.115     1.07 
#> 2 2     Study 2 ZT2    0.00575     0.277    0.0208   0.983   -0.544     0.555
#> 3 3     Study 2 ZT3    0.135       0.243    0.556    0.580   -0.347     0.617
#> 4 4     Study 3 ZT2    0.0422      0.189    0.223    0.824   -0.331     0.416
#> 5 5     Study 3 ZT3    0.145       0.206    0.706    0.481   -0.260     0.551
#> 6 6     Study 3 ZT4   -0.0424      0.179   -0.237    0.813   -0.395     0.311
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
```

To change the sign or the units of the estimates, use
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md),
not [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html). The
dplyr methods keep the vcov row-aligned but do not transform it, so
`mutate(estimate = -estimate)` would flip the estimates while leaving
the covariances inconsistent.
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
updates the vcov to match
($`V \mapsto \mathrm{diag}(s)\,V\,\mathrm{diag}(s)`$), which is what you
want for a sign flip (`by = +1`/`-1`, including a partial flip of some
arms only) or a unit change:

``` r

ev |> rescale_estimates_vcov(by = 100)
#> <estimates_vcov>
#> # 6 estimates with 6x6 vcov matrix
#> 
#> # A tibble: 6 × 11
#>   id    study   term  estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 1     Study 1 ZT2     47.6        29.4    1.62     0.112    -11.5     107. 
#> 2 2     Study 2 ZT2      0.575      27.7    0.0208   0.983    -54.4      55.5
#> 3 3     Study 2 ZT3     13.5        24.3    0.556    0.580    -34.7      61.7
#> 4 4     Study 3 ZT2      4.22       18.9    0.223    0.824    -33.1      41.6
#> 5 5     Study 3 ZT3     14.5        20.6    0.706    0.481    -26.0      55.1
#> 6 6     Study 3 ZT4     -4.24       17.9   -0.237    0.813    -39.5      31.1
#> # ℹ 2 more variables: df <dbl>, outcome <chr>
```

## Correlated estimates with no single regression: bootstrapping

Multi-arm trials are the easy case, because one regression produces all
the arms and [`vcov()`](https://rdrr.io/r/stats/vcov.html) hands us
their covariances. Sometimes estimates are correlated because they share
subjects, but no single regression produces them both. A common design:
every subject takes a survey experiment, and a random third of them also
take a lab experiment. The survey effect and the lab effect are
estimated on overlapping samples, so they covary, but they have
different outcomes and different assignment variables and will not stack
into one model.

The fix is to bootstrap the whole design – resampling *subjects*, so
that the shared-sample dependence is what the replicates reproduce – and
read the covariance off the replicate estimates with
[`cov()`](https://rdrr.io/r/stats/cor.html).
[`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
then wraps those hand-built pieces into an ordinary `estimates_vcov`
object.

``` r

n <- 400
overlap_dat <- data.frame(
  Z_survey = rbinom(n, 1, 0.5),
  in_lab = rbinom(n, 1, 1 / 3)
)
overlap_dat$Z_lab <- if_else(overlap_dat$in_lab == 1, rbinom(n, 1, 0.5), NA)
overlap_dat$Y_survey <- 0.2 * overlap_dat$Z_survey + rnorm(n)
overlap_dat$Y_lab <- 0.5 * overlap_dat$Z_lab + 0.6 * overlap_dat$Y_survey + rnorm(n)

estimate_both <- function(d) {
  c(
    survey = coef(lm_robust(Y_survey ~ Z_survey, data = d))[["Z_survey"]],
    lab = coef(lm_robust(Y_lab ~ Z_lab, data = filter(d, in_lab == 1)))[["Z_lab"]]
  )
}
```

Each iteration draws a bootstrap sample of subjects and re-estimates
both effects on it. Resampling row indices of `overlap_dat` resamples
*subjects*: a subject drawn into the replicate brings both their survey
outcome and, if they were in the lab third, their lab outcome, which is
what carries the shared-sample dependence into the replicate estimates.

``` r

sims <- 200
boots <- matrix(NA_real_, nrow = sims, ncol = 2,
                dimnames = list(NULL, c("survey", "lab")))

for (i in 1:sims) {
  boot_indices <- sample(n, n, replace = TRUE)
  boot_dat <- overlap_dat[boot_indices, ]
  boots[i, ] <- estimate_both(boot_dat)
}

head(boots)
#>           survey       lab
#> [1,]  0.03518958 0.7041343
#> [2,]  0.20013193 0.4622847
#> [3,]  0.04725545 0.3997731
#> [4,] -0.07492539 0.4837416
#> [5,]  0.09572855 0.4192575
#> [6,]  0.01493843 0.1917934
```

The covariance of the replicates is the covariance of the estimates. The
point estimates come from the real data, not from the bootstrap:

``` r

V_boot <- cov(boots)
point <- estimate_both(overlap_dat)
```

The estimates data frame is built by hand, in the same row order as
`V_boot`. Include `std.error` so that
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
has standard errors to rescale:

``` r

boot_estimates <- data.frame(
  study = "Overlapping samples",
  term = names(point),
  estimate = point,
  std.error = sqrt(diag(V_boot))
)

ev_boot <- make_estimates_vcov(boot_estimates, V_boot)
ev_boot
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 5
#>   id    study               term   estimate std.error
#>   <chr> <chr>               <chr>     <dbl>     <dbl>
#> 1 1     Overlapping samples survey   0.0482    0.0983
#> 2 2     Overlapping samples lab      0.475     0.194
get_vcov(ev_boot)
#>             1           2
#> 1 0.009669895 0.001087637
#> 2 0.001087637 0.037517028
```

The off-diagonal element is the shared-subject covariance, which no
single regression would have given us.

The result is an ordinary `estimates_vcov` object, so it binds with
objects built the usual way and pools the same way. The bootstrapped
covariance is carried into the combined matrix, with zeros against the
studies it shares no subjects with:

``` r

combined <- bind_estimates_vcov(ev_boot, ev)
round(get_vcov(combined), 4)
#>        1      2      3      4      5      6      7      8
#> 1 0.0097 0.0011 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
#> 2 0.0011 0.0375 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
#> 3 0.0000 0.0000 0.0862 0.0000 0.0000 0.0000 0.0000 0.0000
#> 4 0.0000 0.0000 0.0000 0.0767 0.0403 0.0000 0.0000 0.0000
#> 5 0.0000 0.0000 0.0000 0.0403 0.0590 0.0000 0.0000 0.0000
#> 6 0.0000 0.0000 0.0000 0.0000 0.0000 0.0358 0.0167 0.0167
#> 7 0.0000 0.0000 0.0000 0.0000 0.0000 0.0167 0.0423 0.0167
#> 8 0.0000 0.0000 0.0000 0.0000 0.0000 0.0167 0.0167 0.0320
```

``` r

combined |> rma_mv_helper(yi = estimate, random = ~ 1 | study)
#> 
#> Multivariate Meta-Analysis Model (k = 8; method: REML)
#> 
#> Variance Components:
#> 
#>             estim    sqrt  nlvls  fixed  factor 
#> sigma^2    0.0000  0.0000      4     no   study 
#> 
#> Test for Heterogeneity:
#> Q(df = 7) = 7.0403, p-val = 0.4247
#> 
#> Model Results:
#> 
#> estimate      se    zval    pval    ci.lb   ci.ub    
#>   0.1250  0.0710  1.7604  0.0783  -0.0142  0.2642  . 
#> 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Two things to keep in mind. The vcov is matched to the estimates **by
position**, so row `i` of the estimates data frame must be row and
column `i` of the matrix; any dimnames on the matrix are discarded and
replaced by the object’s `id`. Building both from the same ordered
vector, as `estimate_both()` does above, makes that automatic. And
[`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
checks that the matrix is square and symmetric, erroring on asymmetry
beyond floating-point noise – a genuinely asymmetric matrix means the
rows and columns are misaligned, which would make the meta-analysis
silently wrong.

## What’s going on under the hood

[`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
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
#>   tidy_obj         glance_obj   vcov_obj     
#>   <list>           <list>       <list>       
#> 1 <tibble [1 × 9]> <df [1 × 7]> <dbl [1 × 1]>
```

We then bind all the prepped fits together with an id variable:

``` r

fits_df
#> # A tibble: 3 × 4
#>   study   tidy_obj         glance_obj   vcov_obj     
#>   <chr>   <list>           <list>       <list>       
#> 1 Study 1 <tibble [1 × 9]> <df [1 × 7]> <dbl [1 × 1]>
#> 2 Study 2 <tibble [2 × 9]> <df [1 × 7]> <dbl [2 × 2]>
#> 3 Study 3 <tibble [3 × 9]> <df [1 × 7]> <dbl [3 × 3]>
```

The
[`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md)
function extracts and unnests the estimates:

``` r

get_estimates_df(fits_df)
#> # A tibble: 6 × 10
#>   study   term  estimate std.error statistic p.value conf.low conf.high    df
#>   <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl>
#> 1 Study 1 ZT2    0.476       0.294    1.62     0.112   -0.115     1.07     48
#> 2 Study 2 ZT2    0.00575     0.277    0.0208   0.983   -0.544     0.555    97
#> 3 Study 2 ZT3    0.135       0.243    0.556    0.580   -0.347     0.617    97
#> 4 Study 3 ZT2    0.0422      0.189    0.223    0.824   -0.331     0.416   196
#> 5 Study 3 ZT3    0.145       0.206    0.706    0.481   -0.260     0.551   196
#> 6 Study 3 ZT4   -0.0424      0.179   -0.237    0.813   -0.395     0.311   196
#> # ℹ 1 more variable: outcome <chr>
```

And the
[`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md)
function extracts each `vcov_obj` and combines them into a
block-diagonal matrix:

``` r

get_vcov(fits_df)
#> 6 x 6 sparse Matrix of class "dsCMatrix"
#>                                                                       
#> [1,] 0.08623141 .          .          .          .          .         
#> [2,] .          0.07670518 0.04029901 .          .          .         
#> [3,] .          0.04029901 0.05898700 .          .          .         
#> [4,] .          .          .          0.03584081 0.01672818 0.01672818
#> [5,] .          .          .          0.01672818 0.04231300 0.01672818
#> [6,] .          .          .          0.01672818 0.01672818 0.03201820
```

The
[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
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
