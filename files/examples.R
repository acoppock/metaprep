#' Clean Workflow Examples with estimates_vcov
#'
#' These examples demonstrate the idiomatic tidyverse workflow using
#' nest_by() + mutate() + reframe() pattern throughout.

library(estimatr)
library(dplyr)
library(metafor)
library(broom)

# ========================================================================
# SETUP: Create Example Data and Prep Fits
# ========================================================================

set.seed(42)
dat <- data.frame(
  Y = rnorm(200),
  Z = factor(sample(c("T0", "T1", "T2"), 200, TRUE)),
  country = sample(c("USA", "UK", "Canada"), 200, TRUE),
  cue_type = sample(c("visual", "auditory"), 200, TRUE),
  study_id = rep(1:4, each = 50)
)

# Fit models using nest_by pattern
prepped_fits <- dat |>
  nest_by(study_id, country, cue_type) |>
  mutate(
    fit_obj = list(lm_robust(Y ~ Z, data = data)),
    prep_obj = list(prep_fit(fit_obj, term = c("ZT1", "ZT2")))
  ) |>
  unnest(prep_obj) |>
  ungroup()

# Create estimates_vcov object
ev <- as_estimates_vcov(prepped_fits)

# ========================================================================
# EXAMPLE 1: Simple Meta-Analysis by Group
# ========================================================================

# Get pooled estimates by cue type
results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1],
    pooled_se = rma_fit$se[1],
    ci_lb = rma_fit$ci.lb[1],
    ci_ub = rma_fit$ci.ub[1],
    p_value = rma_fit$pval[1]
  )

results

# ========================================================================
# EXAMPLE 2: Tidy the Meta-Analysis Results
# ========================================================================

# Use broom::tidy() to get tidy output
tidy_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(tidy(rma_fit))

tidy_results

# ========================================================================
# EXAMPLE 3: Glance at Model Fit Statistics
# ========================================================================

glance_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(glance(rma_fit))

glance_results

# ========================================================================
# EXAMPLE 4: Multiple Grouping Variables
# ========================================================================

results_by_country_and_cue <- ev |>
  nest_by(country, cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1],
    n_estimates = nrow(data$data)
  )

results_by_country_and_cue

# ========================================================================
# EXAMPLE 5: Meta-Regression with Moderators
# ========================================================================

# Run meta-regression within each cue type
moderator_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate, mods = ~ country))
  ) |>
  reframe(tidy(rma_fit))

moderator_results

# ========================================================================
# EXAMPLE 6: Extract Both Tidy and Glance
# ========================================================================

full_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    tidy_obj = list(tidy(rma_fit)),
    glance_obj = list(glance(rma_fit))
  )

# Unnest tidy results
full_results |> 
  select(cue_type, tidy_obj) |> 
  unnest(tidy_obj)

# Unnest glance results
full_results |> 
  select(cue_type, glance_obj) |> 
  unnest(glance_obj)

# ========================================================================
# EXAMPLE 7: Filter THEN Group
# ========================================================================

# Filter to specific countries, then analyze by cue type
ev |>
  filter(country %in% c("USA", "UK")) |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1]
  )

# ========================================================================
# EXAMPLE 8: Add Custom Summaries
# ========================================================================

ev |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    # Meta-analysis summaries
    pooled_est = rma_fit$b[1],
    pooled_se = rma_fit$se[1],
    i2 = rma_fit$I2,
    heterogeneity_p = rma_fit$QEp,
    # Raw data summaries
    n_effects = nrow(data$data),
    mean_raw_est = mean(data$data$estimate),
    sd_raw_est = sd(data$data$estimate)
  )

# ========================================================================
# EXAMPLE 9: Conditional Meta-Analysis
# ========================================================================

# Only run meta-analysis if enough studies
conditional_results <- ev |>
  nest_by(country) |>
  mutate(
    n_estimates = nrow(data$data),
    rma_fit = if (n_estimates >= 3) {
      list(rma_mv_helper(data, yi = estimate))
    } else {
      list(NULL)
    },
    pooled_est = if (!is.null(rma_fit)) rma_fit$b[1] else NA_real_,
    note = if (n_estimates < 3) "Too few estimates" else "OK"
  )

conditional_results

# ========================================================================
# EXAMPLE 10: Compare Fixed vs Random Effects
# ========================================================================

fe_vs_re <- ev |>
  nest_by(cue_type) |>
  mutate(
    fe_fit = list(rma_mv_helper(data, yi = estimate, method = "FE")),
    re_fit = list(rma_mv_helper(data, yi = estimate, method = "REML")),
    fe_est = fe_fit$b[1],
    re_est = re_fit$b[1],
    tau2 = re_fit$tau2,
    i2 = re_fit$I2
  )

fe_vs_re

# ========================================================================
# EXAMPLE 11: Multiple Moderators
# ========================================================================

ev |>
  nest_by(study_id) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate, mods = ~ country + cue_type))
  ) |>
  reframe(tidy(rma_fit))

# ========================================================================
# EXAMPLE 12: Chain Multiple Operations Before Grouping
# ========================================================================

ev |>
  filter(country %in% c("USA", "UK")) |>
  mutate(abs_estimate = abs(estimate)) |>
  filter(abs_estimate > 0.1) |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1]
  )

# ========================================================================
# EXAMPLE 13: Using fix_vcov for Numerical Issues
# ========================================================================

# If you have numerical issues with your vcov matrix
ev_fixed <- fix_vcov(ev, method = "both")

# Then proceed with analysis
ev_fixed |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(tidy(rma_fit))

# ========================================================================
# EXAMPLE 14: Access Individual Fitted Models
# ========================================================================

# Keep the fitted models for later inspection
fitted_models <- ev |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  )

# Access a specific model
summary(fitted_models$rma_fit[[1]])
forest(fitted_models$rma_fit[[1]])

# ========================================================================
# EXAMPLE 15: Complex Workflow with Multiple Steps
# ========================================================================

# A realistic workflow combining multiple operations
final_results <- ev |>
  # Filter
  filter(p.value < 0.05) |>
  # Add derived variables
  mutate(
    effect_size_category = case_when(
      abs(estimate) < 0.2 ~ "small",
      abs(estimate) < 0.5 ~ "medium",
      TRUE ~ "large"
    )
  ) |>
  # Group and analyze
  nest_by(country, effect_size_category) |>
  mutate(
    n_effects = nrow(data$data),
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1],
    pooled_ci_lb = rma_fit$ci.lb[1],
    pooled_ci_ub = rma_fit$ci.ub[1]
  ) |>
  # Filter results
  filter(n_effects >= 2) |>
  # Arrange
  arrange(country, effect_size_category)

final_results

# ========================================================================
# EXAMPLE 16: Using left_join to Add Metadata
# ========================================================================

# Create a lookup table with additional metadata
country_metadata <- data.frame(
  country = c("USA", "UK", "Canada"),
  region = c("North America", "Europe", "North America"),
  population_millions = c(331, 67, 38),
  stringsAsFactors = FALSE
)

# Add metadata with left_join
ev_with_metadata <- ev |>
  left_join(country_metadata, by = "country")

# Now you can use the new columns
ev_with_metadata |>
  nest_by(region) |>
  mutate(
    n_countries = length(unique(data$data$country)),
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1]
  )

# IMPORTANT: left_join must not change the number of rows!
# This would error:
# bad_lookup <- data.frame(
#   country = c("USA", "USA", "UK"),  # USA appears twice!
#   category = c("A", "B", "C")
# )
# ev |> left_join(bad_lookup, by = "country")  # ERROR!

# ========================================================================
# EXAMPLE 17: Using semi_join and anti_join for Filtering
# ========================================================================

# Filter to specific countries using semi_join
countries_to_keep <- data.frame(country = c("USA", "Canada"))

ev_filtered <- ev |>
  semi_join(countries_to_keep, by = "country")

# Remove specific countries using anti_join
countries_to_remove <- data.frame(country = "UK")

ev_filtered2 <- ev |>
  anti_join(countries_to_remove, by = "country")

# These are equivalent to filter(), but useful when you have
# a separate table defining which rows to keep/remove

# ========================================================================
# EXAMPLE 18: Creating from Separate Pieces
# ========================================================================

# If you already extracted estimates and vcov separately
estimates_df <- get_estimates_df(prepped_fits)
vcov_matrix <- get_bdiag_vcov(prepped_fits)

# Combine them into an estimates_vcov object
ev_from_pieces <- estimates_vcov_from_pieces(estimates_df, vcov_matrix)

# Now use it normally
ev_from_pieces |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  )

# ========================================================================
# WORKFLOW SUMMARY
# ========================================================================

# The consistent pattern throughout is:
#
# 1. Start with estimates_vcov object
# 2. Optional: filter(), mutate(), arrange() for preprocessing
# 3. nest_by() to create groups
# 4. mutate() to fit models on each group's data
# 5. reframe(tidy()) or reframe(glance()) to extract results
#
# This pattern is:
# - Consistent with tidymodels and modern tidyverse
# - Readable and explicit
# - Maintains vcov synchronization automatically
# - Avoids ugly [[]] bracket syntax
