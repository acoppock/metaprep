# Package index

## Preparing model fits

Extract tidy estimates, model summaries, and vcov subsets from a fitted
model.

- [`prep_fit()`](https://acoppock.github.io/metaprep/reference/prep_fit.md)
  : Prepare a Model Fit Object for Tidy Extraction

## estimates_vcov objects

Construct the synchronized estimates + vcov object and operate on it
with dplyr verbs.

- [`as_estimates_vcov()`](https://acoppock.github.io/metaprep/reference/as_estimates_vcov.md)
  : Create an estimates_vcov object
- [`estimates_vcov_from_pieces()`](https://acoppock.github.io/metaprep/reference/estimates_vcov_from_pieces.md)
  : Create estimates_vcov from separate estimates and vcov
- [`dplyr-methods`](https://acoppock.github.io/metaprep/reference/dplyr-methods.md)
  : dplyr methods for estimates_vcov objects

## Extracting components

Pull estimates, model-level summaries, or the vcov matrix back out.

- [`get_estimates_df()`](https://acoppock.github.io/metaprep/reference/get_estimates_df.md)
  : Extract estimates from prepped fits or estimates_vcov object
- [`get_glance_df()`](https://acoppock.github.io/metaprep/reference/get_glance_df.md)
  : Extract glance summary from prepped fits or estimates_vcov object
- [`get_vcov()`](https://acoppock.github.io/metaprep/reference/get_vcov.md)
  : Extract variance-covariance matrix from prepped fits or
  estimates_vcov object

## Meta-analysis

Convenience wrappers around metafor that read directly from an
estimates_vcov object.

- [`rma_mv_helper()`](https://acoppock.github.io/metaprep/reference/rma_mv_helper.md)
  : Run rma.mv on an estimates_vcov object
- [`rma_uni_helper()`](https://acoppock.github.io/metaprep/reference/rma_uni_helper.md)
  : Run rma.uni on an estimates_vcov object
