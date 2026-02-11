# Naming Refactor Summary

## Overview

This refactor improves naming consistency across the package by:
1. Converting getter functions to methods that work on both `prepped_fits` and `estimates_vcov` objects
2. Renaming internal "data" references to "estimates" for clarity

## Key Changes

### 1. Unified Getter Methods

All getter functions now work as methods on both object types:

#### get_estimates_df()
```r
# OLD - separate functions
get_estimates_df(prepped_fits)  # unnests tidy_obj
# (no method for estimates_vcov)

# NEW - single method
get_estimates_df(prepped_fits)   # unnests tidy_obj
get_estimates_df(ev)             # extracts $estimates
```

#### get_glance_df()
```r
# OLD
get_glance_df(prepped_fits)     # unnests glance_obj
# (no method for estimates_vcov)

# NEW
get_glance_df(prepped_fits)     # unnests glance_obj
get_glance_df(ev)               # returns NULL with warning (glance not stored)
```

#### get_vcov()
```r
# OLD
get_bdiag_vcov(prepped_fits)    # creates block diagonal
# (no method for estimates_vcov, used internal accessor)

# NEW - single method
get_vcov(prepped_fits)           # creates block diagonal
get_vcov(ev)                     # extracts $vcov
```

### 2. Internal Field Renaming

The `estimates_vcov` object structure changed:

```r
# OLD structure
list(
  data = <tibble of estimates>,
  vcov = <matrix>,
  row_map = <integer vector>
)

# NEW structure
list(
  estimates = <tibble of estimates>,
  vcov = <matrix>,
  row_map = <integer vector>
)
```

**Rationale**: "estimates" is more descriptive than "data" and clarifies what the tibble contains.

## Files Modified

### estimates_vcov.R
- `new_estimates_vcov()`: parameter renamed from `data` to `estimates`
- `print.estimates_vcov()`: uses `x$estimates` instead of `x$data`
- `as.data.frame.estimates_vcov()`: uses `x$estimates`
- `as_tibble.estimates_vcov()`: uses `x$estimates`
- `get_estimates()`: new generic with methods for `data.frame` and `estimates_vcov`
- `get_glance()`: new generic with methods for `data.frame` and `estimates_vcov`
- `get_vcov()`: expanded to generic with methods for both classes
- `as_estimates_vcov()`: now calls `get_estimates()` and `get_vcov()`
- `fix_vcov.estimates_vcov()`: uses `vcov$estimates`

### estimates_vcov_dplyr.R
All dplyr methods updated to use `$estimates` instead of `$data`:
- `filter.estimates_vcov()`
- `slice.estimates_vcov()`
- `arrange.estimates_vcov()`
- `mutate.estimates_vcov()`
- `select.estimates_vcov()`
- `rename.estimates_vcov()`
- `relocate.estimates_vcov()`
- `pull.estimates_vcov()`
- `nest_by.estimates_vcov()`
- `left_join.estimates_vcov()`
- `semi_join.estimates_vcov()`
- `anti_join.estimates_vcov()`

Variable names also updated for consistency:
- `filtered_data` → `filtered_estimates`
- `sliced_data` → `sliced_estimates`
- `arranged_data` → `arranged_estimates`
- `new_data` → `new_estimates`
- `indexed_data` → `indexed_estimates`
- `joined_data` → `joined_estimates`
- `original_data` → `original_estimates`

### rma_helpers.R
- `rma_mv_helper.estimates_vcov()`: uses `object$estimates` instead of `object$data`
- `rma_uni_helper.estimates_vcov()`: uses `object$estimates` instead of `object$data`

## Migration Guide

### For Package Users

**Function Name Changes:**
```r
# OLD
vcov_matrix <- get_bdiag_vcov(prepped_fits)

# NEW
vcov_matrix <- get_vcov(prepped_fits)

# These stay the same:
estimates_df <- get_estimates_df(prepped_fits)  # unchanged
glance_df <- get_glance_df(prepped_fits)        # unchanged

# New: can now use on estimates_vcov objects too
estimates_df <- get_estimates_df(ev)
glance_df <- get_glance_df(ev)  # warns, returns NULL
vcov_matrix <- get_vcov(ev)
```

### For Package Developers

**Use accessor functions instead of direct field access:**
```r
# OLD (breaks)
ev$estimates
ev$estimates$term
nrow(ev$estimates)

# NEW (use accessor)
get_estimates_df(ev)
get_estimates_df(ev)$term
nrow(get_estimates_df(ev))
```

## Example Workflow (Before and After)

### Before
```r
library(estimatr)
library(dplyr)

# Fit models
prepped_fits <- dat |>
  nest_by(study) |>
  mutate(
    fit = list(lm_robust(Y ~ Z, data = data)),
    prep = list(prep_fit(fit, term = "ZT1"))
  ) |>
  unnest(prep)

# Extract pieces separately - different function names
estimates_df <- get_estimates_df(prepped_fits)
glance_df <- get_glance_df(prepped_fits)
vcov_matrix <- get_bdiag_vcov(prepped_fits)  # Different function!

# Or create estimates_vcov object
ev <- as_estimates_vcov(prepped_fits)
# No way to extract estimates or vcov from ev using functions
```

### After
```r
library(estimatr)
library(dplyr)

# Fit models (same)
prepped_fits <- dat |>
  nest_by(study) |>
  mutate(
    fit = list(lm_robust(Y ~ Z, data = data)),
    prep = list(prep_fit(fit, term = "ZT1"))
  ) |>
  unnest(prep)

# Extract pieces - consistent function names
estimates_df <- get_estimates_df(prepped_fits)
glance_df <- get_glance_df(prepped_fits)
vcov_matrix <- get_vcov(prepped_fits)

# Or create estimates_vcov object
ev <- as_estimates_vcov(prepped_fits)
estimates_df <- get_estimates_df(ev)  # Same function name!
vcov_matrix <- get_vcov(ev)           # Same function name!
```

## Q&A

**Q: Why keep the _df suffix for get_estimates_df and get_glance_df?**
A: To avoid naming conflicts with modelsummary's `get_estimates()` function which is commonly used. The _df suffix is clear and indicates these return data frames.

**Q: Why not have _df on get_vcov?**
A: `vcov` is already clearly a matrix (the vc stands for variance-covariance), and there's no common function conflict. Plus it matches base R's `vcov()` naming convention.

**Q: What if I need to access the estimates field directly?**
A: Use `get_estimates_df(ev)` instead. This is better practice as it uses the public API and will be maintained across future changes.

**Q: Why rename "data" to "estimates"?**
A: More descriptive and specific. The object contains estimates, not generic data. This makes the code more self-documenting.
