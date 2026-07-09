# Changelog

## metaprep 0.1.0

- First release.
- [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
  extracts tidy estimates, a
  [`glance()`](https://generics.r-lib.org/reference/glance.html)
  summary, and the relevant variance-covariance submatrix from a fitted
  model, with exact or regex term matching and multivariate-model
  handling.
- [`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
  and
  [`estimates_vcov_from_pieces()`](https://alexandercoppock.com/metaprep/reference/estimates_vcov_from_pieces.md)
  build an `estimates_vcov` object that keeps coefficient estimates and
  their block-diagonal vcov matrix synchronized.
- dplyr methods
  ([`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html),
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html), and
  the join verbs) operate on `estimates_vcov` objects while keeping the
  vcov matrix in sync.
- [`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md),
  [`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md),
  and
  [`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md)
  pull the components back out of either a prepped-fits tibble or an
  `estimates_vcov` object.
- [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  and
  [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  wrap
  [`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
  and
  [`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html),
  reading the estimates and vcov directly from an `estimates_vcov`
  object.
