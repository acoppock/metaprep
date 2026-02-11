# Quick Start Guide: estimates_vcov Class

## The Problem

You have tidy estimates in a data frame and a variance-covariance matrix, but when 
you do normal dplyr operations (filter, nest_by), the vcov matrix gets out of sync.

```r
# OLD WAY - Manual and error-prone
estimates_df <- get_estimates_df(prepped_fits)
vcov_matrix <- get_bdiag_vcov(prepped_fits)

# If you filter...
filtered_df <- estimates_df |> filter(country == "USA")
# ...you have to manually subset vcov too
filtered_idx <- which(estimates_df$country == "USA")
filtered_vcov <- vcov_matrix[filtered_idx, filtered_idx]

# Easy to make mistakes!
```

## The Solution

The `estimates_vcov` S3 class keeps data and vcov synchronized automatically.

```r
# NEW WAY - Automatic synchronization
ev <- as_estimates_vcov(prepped_fits)

# Now filtering just works!
filtered_ev <- ev |> filter(country == "USA")
# vcov is automatically subsetted to match

# And you can pipe directly to meta-analysis
filtered_ev |> rma_mv_helper(yi = estimate)
```

## Your Target Workflow

```r
library(metaprep)
library(estimatr)
library(dplyr)
library(broom)

# 1. Fit models using nest_by pattern
prepped_fits <- dat |>
  nest_by(study, country) |>
  mutate(
    fit_obj = list(lm_robust(Y ~ Z, data = data)),
    prep_obj = list(prep_fit(fit_obj, term = "Z"))
  ) |>
  unnest(prep_obj)

# 2. Create estimates_vcov object
ev <- as_estimates_vcov(prepped_fits)

# 3. Use nest_by + mutate + reframe pattern
ev |>
  filter(country == "USA") |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(tidy(rma_fit))
```

## What Works

### Row operations (maintain synchronization):
- `filter()` - Subsets data and vcov
- `slice()` - Subsets by position
- `arrange()` - Reorders data and vcov
- `semi_join()` - Filters rows based on another table
- `anti_join()` - Filters out rows based on another table

### Column operations (vcov unchanged):
- `mutate()` - Adds/modifies columns
- `select()` - Selects columns
- `rename()` - Renames columns
- `relocate()` - Reorders columns
- `left_join()` - Adds columns from another table (must not change row count!)

### Grouping operation:
- `nest_by()` - Creates rowwise tibble with nested estimates_vcov objects

### NOT supported (would break synchronization):
- `right_join()`, `inner_join()`, `full_join()` - These can add/remove rows
- Use `left_join()` for adding columns, `filter()` for removing rows

### Meta-analysis helpers:
- `rma_mv_helper()` - Wrapper for metafor::rma.mv()
- `rma_uni_helper()` - Wrapper for metafor::rma.uni()
- Both work seamlessly with rowwise tibbles from nest_by()

### Utility:
- `fix_vcov()` - Fix floating-point issues in vcov matrices
- `estimates_vcov_from_pieces()` - Create from separate estimates_df and vcov_matrix

## The Idiomatic Pattern

Following tidymodels and modern tidyverse conventions:

```r
# Pattern: nest_by() + mutate() + reframe()

# Step 1: Group with nest_by()
ev |>
  nest_by(country, cue_type) |>
  
  # Step 2: Fit models with mutate()
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  
  # Step 3: Extract results with reframe()
  reframe(tidy(rma_fit))
```

This pattern is:
- Consistent throughout your analysis
- Readable and explicit
- Maintains vcov synchronization automatically
- Integrates with broom for tidy() and glance()

## Common Workflows

### Simple pooling by group:
```r
ev |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1],
    pooled_se = rma_fit$se[1]
  )
```

### Meta-regression with moderators:
```r
ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate, mods = ~ country))
  ) |>
  reframe(tidy(rma_fit))
```

### Filter then analyze:
```r
ev |>
  filter(p.value < 0.05) |>
  nest_by(study_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(glance(rma_fit))
```

### Conditional analysis:
```r
ev |>
  nest_by(country) |>
  mutate(
    n_effects = nrow(data$data),
    rma_fit = if (n_effects >= 3) {
      list(rma_mv_helper(data, yi = estimate))
    } else {
      list(NULL)
    }
  )
```

## Key Design Principles

### Naming Convention
All output columns use `_obj` suffix to distinguish from functions:
- `tidy_obj` (not `tidy`) - output of tidy() function
- `glance_obj` (not `glance`) - output of glance() function  
- `vcov_obj` (not `vcov`) - output of vcov() function
- `fit_obj`, `prep_obj`, etc. - other objects

### Trust the Input
The package trusts your vcov matrices by default, but provides `fix_vcov()` 
to handle common floating-point issues:

```r
# Fix symmetry and negative eigenvalues
ev_fixed <- fix_vcov(ev, method = "both")

# Just symmetrize
vcov_fixed <- fix_vcov(my_vcov, method = "symmetrize")
```

### Consistent Grouping Pattern
Always use `nest_by()` for grouping, not `group_by()`. This keeps the 
package consistent and follows tidymodels conventions.

## Installation in Your Package

See `RECOMMENDATIONS.md` for detailed integration instructions.

## Examples

See `examples.R` for 15 comprehensive examples covering:
- Basic meta-analysis
- Meta-regression with moderators
- Filtering and preprocessing
- Conditional analysis
- Fixed vs random effects
- Complex multi-step workflows

## Next Steps

1. Review `examples.R` for comprehensive usage patterns
2. See `RECOMMENDATIONS.md` for package integration details
3. Check `test-estimates-vcov.R` for test suite examples
