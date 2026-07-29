# Package index

## Overview

Start here.

- [`metaprep`](https://alexandercoppock.com/metaprep/reference/metaprep-package.md)
  [`metaprep-package`](https://alexandercoppock.com/metaprep/reference/metaprep-package.md)
  : metaprep: Tidy Preparation of Dependent Effect Estimates for
  Meta-Analysis
- [`estimates_vcov`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
  [`estimates_vcov-class`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
  [`print.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
  [`as.data.frame.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
  [`as_tibble.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/estimates_vcov.md)
  : The estimates_vcov object

## Preparing model fits

Extract tidy estimates, model summaries, and vcov subsets from a fitted
model.

- [`prep_fit()`](https://alexandercoppock.com/metaprep/reference/prep_fit.md)
  : Prepare a Model Fit Object for Tidy Extraction

## estimates_vcov objects

Construct the synchronized estimates + vcov object and operate on it
with dplyr verbs.

- [`as_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/as_estimates_vcov.md)
  : Create an estimates_vcov object
- [`make_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/make_estimates_vcov.md)
  : Create an estimates_vcov from estimates and a vcov you already have
- [`bind_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/bind_estimates_vcov.md)
  : Combine estimates_vcov objects
- [`rescale_estimates_vcov()`](https://alexandercoppock.com/metaprep/reference/rescale_estimates_vcov.md)
  : Sign-flip or rescale an estimates_vcov object
- [`dplyr-methods`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`filter.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`slice.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`slice_head.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`slice_tail.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`arrange.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`mutate.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`select.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`rename.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`relocate.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`pull.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`nest_by.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`left_join.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`right_join.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`inner_join.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`full_join.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`semi_join.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  [`anti_join.estimates_vcov`](https://alexandercoppock.com/metaprep/reference/dplyr-methods.md)
  : dplyr methods for estimates_vcov objects

## Extracting components

Pull estimates, model-level summaries, or the vcov matrix back out.

- [`get_estimates_df()`](https://alexandercoppock.com/metaprep/reference/get_estimates_df.md)
  : Extract estimates from prepped fits or estimates_vcov object
- [`get_glance_df()`](https://alexandercoppock.com/metaprep/reference/get_glance_df.md)
  : Extract glance summary from prepped fits or estimates_vcov object
- [`get_vcov()`](https://alexandercoppock.com/metaprep/reference/get_vcov.md)
  : Extract variance-covariance matrix from prepped fits or
  estimates_vcov object

## Meta-analysis

Convenience wrappers around metafor that read directly from an
estimates_vcov object.

- [`rma_mv_helper()`](https://alexandercoppock.com/metaprep/reference/rma_mv_helper.md)
  : Run rma.mv on an estimates_vcov object
- [`rma_uni_helper()`](https://alexandercoppock.com/metaprep/reference/rma_uni_helper.md)
  : Run rma.uni on an estimates_vcov object
