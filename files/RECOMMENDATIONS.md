# Package Integration Recommendations

## 1. Files to Add to Your Package

Copy these files to your package's `R/` directory:

1. **estimates_vcov.R** - Core S3 class and constructors
2. **estimates_vcov_dplyr.R** - dplyr method implementations
3. **rma_helpers.R** - Meta-analysis convenience wrappers
4. **prep_fit.R** - Updated prep_fit function
5. **get_extraction_functions.R** - Updated extraction functions

## 2. Namespace Exports

Add to your `NAMESPACE` file:

```r
# Core class
export(as_estimates_vcov)
export(get_data)
export(get_vcov)
export(fix_vcov)

# S3 methods for estimates_vcov
S3method(print, estimates_vcov)
S3method(as.data.frame, estimates_vcov)
S3method(as_tibble, estimates_vcov)

# dplyr methods
S3method(filter, estimates_vcov)
S3method(slice, estimates_vcov)
S3method(arrange, estimates_vcov)
S3method(mutate, estimates_vcov)
S3method(select, estimates_vcov)
S3method(rename, estimates_vcov)
S3method(relocate, estimates_vcov)
S3method(pull, estimates_vcov)
S3method(nest_by, estimates_vcov)
S3method(ungroup, estimates_vcov)

# Meta-analysis helpers
export(rma_mv_helper)
export(rma_uni_helper)
S3method(rma_mv_helper, estimates_vcov)
S3method(rma_mv_helper, list)
S3method(rma_uni_helper, estimates_vcov)
S3method(rma_uni_helper, list)

# Vcov fixing
S3method(fix_vcov, matrix)
S3method(fix_vcov, estimates_vcov)

# Extraction functions
export(prep_fit)
export(get_estimates_df)
export(get_glance_df)
export(get_bdiag_vcov)
```

## 3. Package Dependencies

### Imports (required):
```r
Imports:
    dplyr (>= 1.0.0),
    tidyr (>= 1.0.0),
    tibble (>= 3.0.0),
    rlang (>= 0.4.0),
    broom (>= 0.7.0),
    Matrix,
    stringr
```

### Suggests (optional):
```r
Suggests:
    metafor (>= 3.0.0),
    estimatr,
    testthat (>= 3.0.0)
```

## 4. Documentation

### Package-level Documentation

Create `R/metaprep-package.R`:

```r
#' metaprep: Tidy Meta-Analysis Preparation
#'
#' @description
#' Provides tools for preparing estimates from fitted models for meta-analysis,
#' with automatic synchronization of variance-covariance matrices through tidy
#' data operations.
#'
#' @section Main Functions:
#' * [prep_fit()] - Extract estimates from fitted models
#' * [as_estimates_vcov()] - Create synchronized estimate+vcov objects
#' * [rma_mv_helper()] - Convenience wrapper for metafor::rma.mv()
#'
#' @section Workflow:
#' The typical workflow is:
#' 1. Fit models using nest_by() pattern
#' 2. Extract estimates with prep_fit()
#' 3. Create estimates_vcov object with as_estimates_vcov()
#' 4. Use dplyr verbs (filter, mutate, nest_by) as normal
#' 5. Run meta-analysis with rma_mv_helper()
#'
#' @docType package
#' @name metaprep-package
NULL
```

### Vignette

Create `vignettes/getting-started.Rmd`:

```rmd
---
title: "Getting Started with metaprep"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Getting Started with metaprep}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[Include examples.R content here]
```

## 5. Testing

Create `tests/testthat/test-estimates-vcov.R` (see separate file).

Key test categories:
- Construction and validation
- Filter and slice operations
- Arrange operations
- Mutate and column operations
- nest_by operations
- Integration with rma_mv_helper
- fix_vcov functionality
- Edge cases (empty results, single rows)

## 6. README Updates

Add to your README.md:

```markdown
## Installation

```r
# Install from GitHub
remotes::install_github("yourusername/metaprep")
```

## Quick Example

```r
library(metaprep)
library(estimatr)
library(dplyr)
library(broom)

# Fit models across groups
prepped_fits <- dat |>
  nest_by(study, country) |>
  mutate(
    fit_obj = list(lm_robust(Y ~ Z, data = data)),
    prep_obj = list(prep_fit(fit_obj, term = "Z"))
  ) |>
  unnest(prep_obj)

# Create estimates_vcov object
ev <- as_estimates_vcov(prepped_fits)

# Run meta-analysis by group
ev |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(tidy(rma_fit))
```

## Key Features

- **Automatic synchronization**: vcov matrix stays in sync through filter(), slice(), arrange()
- **Tidy integration**: Works seamlessly with dplyr verbs and broom functions
- **Meta-analysis helpers**: Convenient wrappers for metafor functions
- **Numerical stability**: Built-in tools for fixing floating-point issues
```

## 7. Design Decisions

### Why `_obj` suffix?
Distinguishes between:
- Functions: `tidy()`, `glance()`, `vcov()`
- Objects: `tidy_obj`, `glance_obj`, `vcov_obj`

This prevents confusion when reading code and makes it clear that you're
working with stored outputs, not calling functions.

### Why `nest_by()` only?
- Consistency: One pattern throughout
- Clarity: Explicit about creating nested structure
- Tidymodels alignment: Matches modern tidyverse conventions
- Simplicity: Fewer patterns to learn and maintain

### Why trust the input?
- Users know their data best
- Imposing restrictions could break valid use cases
- `fix_vcov()` provides opt-in fixing when needed

### Why rowwise tibbles?
- Natural integration with mutate()
- Clean syntax (no `[[]]` brackets)
- Standard tidyverse pattern
- Works seamlessly with list-columns

## 8. Migration Guide for Existing Users

If you have existing code using the old functions:

```r
# OLD: Separate extraction
estimates_df <- get_estimates_df(prepped_fits)
vcov_matrix <- get_bdiag_vcov(prepped_fits)

# NEW: Combined object
ev <- as_estimates_vcov(prepped_fits)
```

```r
# OLD: Manual vcov subsetting
filtered_est <- estimates_df |> filter(country == "USA")
idx <- which(estimates_df$country == "USA")
filtered_vcov <- vcov_matrix[idx, idx]

# NEW: Automatic synchronization
filtered_ev <- ev |> filter(country == "USA")
```

The old functions still work if you need them for backward compatibility!

## 9. Common Patterns

### Pattern 1: Simple pooling
```r
ev |>
  nest_by(group_var) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1]
  )
```

### Pattern 2: Get tidy results
```r
ev |>
  nest_by(group_var) |>
  mutate(rma_fit = list(rma_mv_helper(data, yi = estimate))) |>
  reframe(tidy(rma_fit))
```

### Pattern 3: Get model fit statistics
```r
ev |>
  nest_by(group_var) |>
  mutate(rma_fit = list(rma_mv_helper(data, yi = estimate))) |>
  reframe(glance(rma_fit))
```

### Pattern 4: Meta-regression
```r
ev |>
  nest_by(group_var) |>
  mutate(rma_fit = list(rma_mv_helper(data, yi = estimate, mods = ~ moderator))) |>
  reframe(tidy(rma_fit))
```

### Pattern 5: Filter then analyze
```r
ev |>
  filter(condition) |>
  nest_by(group_var) |>
  mutate(rma_fit = list(rma_mv_helper(data, yi = estimate))) |>
  reframe(tidy(rma_fit))
```

## 10. Advanced Usage

### Custom vcov matrices
```r
# Use a different vcov matrix
ev |>
  nest_by(group) |>
  mutate(
    custom_vcov = list(compute_custom_vcov(data)),
    rma_fit = list(rma_mv_helper(data, yi = estimate, V = custom_vcov))
  )
```

### Fixed vs random effects comparison
```r
ev |>
  nest_by(group) |>
  mutate(
    fe = list(rma_mv_helper(data, yi = estimate, method = "FE")),
    re = list(rma_mv_helper(data, yi = estimate, method = "REML")),
    fe_est = fe$b[1],
    re_est = re$b[1]
  )
```

### Conditional analysis
```r
ev |>
  nest_by(group) |>
  mutate(
    n = nrow(data$data),
    rma_fit = if (n >= 3) list(rma_mv_helper(data, yi = estimate)) else list(NULL),
    pooled_est = if (!is.null(rma_fit)) rma_fit$b[1] else NA_real_
  )
```

## 11. Troubleshooting

### Issue: "vcov matrix is not positive definite"
```r
# Solution: Use fix_vcov()
ev_fixed <- fix_vcov(ev, method = "near_psd")
```

### Issue: "Input must contain list-columns named tidy_obj and vcov_obj"
```r
# Check your prep_fit output
names(prepped_fits)
# Should include: tidy_obj, glance_obj, vcov_obj
```

### Issue: nest_by() creates empty groups
```r
# Check for NAs in grouping variables
ev |> 
  filter(!is.na(group_var)) |>
  nest_by(group_var)
```

## 12. Performance Considerations

- For large datasets (>10,000 estimates), consider working with subsets
- Block-diagonal vcov matrices are memory efficient
- `nest_by()` creates one copy per group - watch memory for many groups
- Use `method = "FE"` in rma.mv() when you don't need random effects (faster)

## 13. Future Enhancements

Potential additions (feedback welcome):
- Support for other model types beyond lm_robust
- Parallel processing for nest_by() + mutate() patterns
- Forest plot methods for estimates_vcov objects
- Integration with other meta-analysis packages

## 14. Getting Help

- File issues at: https://github.com/yourusername/metaprep/issues
- See examples: `?as_estimates_vcov` or browse `examples.R`
- Check vignettes: `vignette("getting-started", package = "metaprep")`
