# dplyr methods for estimates_vcov objects

These methods allow standard dplyr operations on estimates_vcov objects
while keeping the variance-covariance matrix synchronized with the data.

Supported operations:

- Row operations that maintain sync:
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

- Column operations that don't change rows:
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html)

- Grouping operation:
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html)
  (returns rowwise tibble with nested estimates_vcov)

## Arguments

- .data, x:

  An `estimates_vcov` object.

- y:

  A data frame to join against.

- ...:

  Passed on to the corresponding dplyr verb.

- .preserve, .by_group, .key, .keep, n, prop, var, name:

  Passed on to the corresponding dplyr verb.

- by, copy, suffix, keep, na_matches, multiple, unmatched, relationship:

  Passed on to the corresponding dplyr join.

- .before, .after:

  Passed on to
  [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html).

## Value

An `estimates_vcov` object, except for
[`pull()`](https://dplyr.tidyverse.org/reference/pull.html), which
returns a vector, and
[`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html), which
returns a rowwise tibble whose `.key` column holds one `estimates_vcov`
object per group.

## Details

These methods keep the vcov **row-aligned** (subsetting and reordering
the rows also subset/reorder the matrix), but they never **transform**
the matrix. In particular
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) can
change the *values* of a column without touching the vcov, so
`mutate(estimate = -estimate)` (or any rescaling of `estimate`) leaves
the vcov inconsistent with the estimates. To sign-flip or rescale
estimates and keep the covariances valid, use
[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md),
which updates the vcov as \\\mathrm{diag}(s)\\ V\\ \mathrm{diag}(s)\\.

Joins that could change the row set are refused rather than allowed to
desynchronize the object:
[`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
and
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
can drop or add rows, and
[`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
changes which rows are kept.
[`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
is supported, and errors if the join turns out to duplicate rows. Use
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) when the
intent is to remove estimates.

## See also

[`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
to transform estimate values and the vcov together, and
[estimates_vcov](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
for what the object guarantees.

Other estimates_vcov objects:
[`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md),
[`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md),
[`estimates_vcov`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md),
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

# Row operations subset or reorder the vcov along with the estimates
dim(get_vcov(ev))
#> [1] 3 3
dim(get_vcov(filter(ev, study == "study_2")))
#> [1] 2 2
rownames(get_vcov(arrange(ev, estimate)))
#> [1] "3" "2" "1"

# Column operations leave the vcov alone, because the rows cannot change
ev |> mutate(abs_estimate = abs(estimate))
#> <estimates_vcov>
#> # 3 estimates with 3x3 vcov matrix
#> 
#> # A tibble: 3 × 8
#>   id    study   term  estimate std.error statistic p.value abs_estimate
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>        <dbl>
#> 1 1     study_1 ZT1    0.0503      0.223    0.226   0.822       0.0503 
#> 2 2     study_2 ZT1    0.00305     0.238    0.0128  0.990       0.00305
#> 3 3     study_2 ZT2   -0.440       0.238   -1.85    0.0679      0.440  
ev |> select(id, study, term, estimate)
#> <estimates_vcov>
#> # 3 estimates with 3x3 vcov matrix
#> 
#> # A tibble: 3 × 4
#>   id    study   term  estimate
#>   <chr> <chr>   <chr>    <dbl>
#> 1 1     study_1 ZT1    0.0503 
#> 2 2     study_2 ZT1    0.00305
#> 3 3     study_2 ZT2   -0.440  

# nest_by() gives one self-contained object per group, each with its own vcov
nested <- ev |> nest_by(study)
nested$data[[2]]
#> <estimates_vcov>
#> # 2 estimates with 2x2 vcov matrix
#> 
#> # A tibble: 2 × 6
#>   id    term  estimate std.error statistic p.value
#>   <chr> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 2     ZT1    0.00305     0.238    0.0128  0.990 
#> 2 3     ZT2   -0.440       0.238   -1.85    0.0679
dim(get_vcov(nested$data[[2]]))
#> [1] 2 2

# left_join() adds columns; a join that duplicated rows would be an error
ev |> left_join(data.frame(study = c("study_1", "study_2"),
                           region = c("north", "south")), by = "study")
#> <estimates_vcov>
#> # 3 estimates with 3x3 vcov matrix
#> 
#> # A tibble: 3 × 8
#>   id    study   term  estimate std.error statistic p.value region
#>   <chr> <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl> <chr> 
#> 1 1     study_1 ZT1    0.0503      0.223    0.226   0.822  north 
#> 2 2     study_2 ZT1    0.00305     0.238    0.0128  0.990  south 
#> 3 3     study_2 ZT2   -0.440       0.238   -1.85    0.0679 south 

# Study 2's two arms covary: that is the entry a sign flip has to update.
get_vcov(ev)[2, 3]
#> [1] 0.02836191

# mutate() does NOT transform the vcov, so this desynchronizes the object:
get_vcov(mutate(ev, estimate = -estimate))[2, 3]  # unchanged, now wrong
#> [1] 0.02836191

# rescale_estimates_vcov() updates it. Flipping one arm and not the other
# flips the sign of their covariance, which is the case easiest to get wrong:
get_vcov(rescale_estimates_vcov(ev, by = if_else(id == "2", -1, 1)))[2, 3]
#> [1] -0.02836191
```
