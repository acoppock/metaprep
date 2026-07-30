# The estimates_vcov object

An `estimates_vcov` object holds a set of coefficient estimates together
with the variance-covariance matrix that describes their dependence, and
keeps the two aligned as you reshape them. It is the object the rest of
the package is built around:
[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
and
[`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
create one, the
[dplyr-methods](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
reshape one, and
[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
pools one.

The problem it solves is bookkeeping. A meta-analysis of multi-arm
trials needs a block-diagonal vcov whose row `i` corresponds to estimate
`i`, and every filter, sort, or subset of the estimates has to be
applied to the matrix in the same way. Doing that by hand is where a
meta-analysis goes silently wrong: nothing about a misaligned matrix
looks wrong, and the pooled result is simply the wrong number.

## Structure

The object is a list of three components:

- `estimates`:

  A tibble of estimates, one row each, with an `id` column first.
  Carries whatever columns
  [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
  produced (`term`, `estimate`, `std.error`, and so on) plus any you
  have added.

- `vcov`:

  A square symmetric matrix, one row and column per estimate, in the
  same order as the rows of `estimates`, with `dimnames` equal to the
  `id` values.

- `row_map`:

  Internal bookkeeping. See "Public interface" below.

## Public interface

`estimates` and `vcov` are part of the public interface. Read them
however suits the code you are writing: `ev$estimates` and `ev$vcov`
directly, or
[`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md)
and
[`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md).
Both will keep working.

What is guaranteed about `vcov` is its *content and shape*, not its
storage class: square, symmetric, finite, one row and column per
estimate in the same order as `estimates`, `dimnames` equal to `id`.
Ordinary matrix operations (`V[i, j]`, `V[idx, idx]`, `diag(V)`,
`upper.tri(V)`, `w %*% V %*% w`) are the supported way to use it, and
they behave the same whether the matrix is stored densely or sparsely.
Code that tests the storage class itself, for instance with
[`is.matrix()`](https://rdrr.io/r/base/matrix.html), is relying on
something the object does not promise.

`row_map` is **internal** and may change without notice. It records
which rows of a parent object the current rows came from, but the verbs
do not agree on what "parent" means:
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and the
filtering joins set it to positions within the object they were handed,
while [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
composes it through. Do not build on it. Use `id` when you need a stable
per-estimate label; it is preserved through subsetting for exactly that
purpose.

## The id column

Every object carries an `id` column, added when it is built, and the
vcov's `dimnames` match it. `id` is what links an estimate to its row
and column of the matrix, and it is preserved through subsetting, so a
filtered object keeps the ids it had rather than being renumbered from
one. That is what makes `id` usable as a random-effect grouping term:
`random = ~ 1 | id` gives each estimate its own random effect.
([`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md)
is the exception: it renumbers `id` across the combined object, since
the inputs each started from one.)

## What stays synchronized, and what does not

The dplyr methods keep the vcov **row-aligned**. Dropping or reordering
estimates drops or reorders the matching rows and columns of the matrix,
so [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
[`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
and
[`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
are all safe. Verbs that cannot change the row set
([`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
[`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html))
leave the matrix untouched. Verbs that would add or drop rows
unpredictably
([`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html))
are refused rather than silently desynchronizing the object.

What the dplyr methods never do is **transform** the matrix.
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) can
change the values in the `estimate` column while leaving the vcov
exactly as it was, so `mutate(estimate = -estimate)` produces an object
whose covariances no longer describe its estimates. Use
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
to sign-flip or rescale, which updates the matrix to match.

## Guarantees

Every object that gets built satisfies three conditions, checked at
construction: the estimates and the vcov have the same number of rows;
the vcov is square and symmetric (asymmetry beyond floating-point noise
is an error, not something to average away); and the vcov is finite. A
non-finite covariance usually means a rank-deficient fit returned
coefficients without usable standard errors, and such an estimate must
not be pooled as if it carried uncertainty.

## See also

[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
and
[`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
to build one,
[dplyr-methods](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
for the verbs it supports, and
[`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
to pool it.

Other estimates_vcov objects:
[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md),
[`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md),
[`dplyr-methods`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md),
[`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md),
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)

## Examples

``` r
library(dplyr)

set.seed(1)
dat_1 <- data.frame(Y = rnorm(60), Z = factor(rep(c("T0", "T1"), each = 30)))
dat_2 <- data.frame(Y = rnorm(90), Z = factor(rep(c("T0", "T1", "T2"), each = 30)))

ev <- as_estimates_vcov(bind_rows(
  study_1 = prep_fit(lm(Y ~ Z, dat_1), term = "ZT1"),
  study_2 = prep_fit(lm(Y ~ Z, dat_2), term = c("ZT1", "ZT2")),
  .id = "study"
))
ev
#> <estimates_vcov>
#> # 3 estimates with 3x3 vcov matrix
#> 
#> # A tibble: 3 × 7
#>   id    study   term  estimate std.error statistic p.value
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 1     study_1 ZT1    0.0503      0.223    0.226   0.822 
#> 2 2     study_2 ZT1    0.00305     0.238    0.0128  0.990 
#> 3 3     study_2 ZT2   -0.440       0.238   -1.85    0.0679

# The id column indexes the vcov
get_estimates_df(ev)$id
#> [1] "1" "2" "3"
rownames(get_vcov(ev))
#> [1] "1" "2" "3"

# Study 2's two arms covary; study 1 is independent of both
round(get_vcov(ev), 5)
#> 3 x 3 sparse Matrix of class "dsCMatrix"
#>         1       2       3
#> 1 0.04955 .       .      
#> 2 .       0.05672 0.02836
#> 3 .       0.02836 0.05672

# Subsetting keeps the two in step, and keeps the original ids
ev_2 <- filter(ev, study == "study_2")
get_estimates_df(ev_2)$id
#> [1] "2" "3"
dim(get_vcov(ev_2))
#> [1] 2 2
```
