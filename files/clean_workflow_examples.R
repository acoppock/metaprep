#' Clean Workflow Examples - No Ugly Brackets!
#'
#' These examples show the idiomatic tidyverse way to work with
#' estimates_vcov objects using nest_by() and mutate().

library(estimatr)
library(dplyr)
library(metafor)
library(broom)  # for tidy() and glance()

# ---- Setup Example Data ----

set.seed(42)
dat <- data.frame(
  Y = rnorm(200),
  Z = factor(sample(c("T0", "T1", "T2"), 200, TRUE)),
  country = sample(c("USA", "UK", "Canada"), 200, TRUE),
  cue_type = sample(c("visual", "auditory"), 200, TRUE)
)

# Fit multiple models
fits <- dat |>
  group_by(country, cue_type) |>
  group_modify(~ {
    fit <- lm_robust(Y ~ Z, data = .x)
    prep_fit(fit, term = c("ZT1", "ZT2"))
  })

# Create estimates_vcov object
ev <- as_estimates_vcov(fits)

# ========================================================================
# MODERN TIDYVERSE APPROACH: nest_by() + mutate()
# ========================================================================

# ---- Example 1: Simple Meta-Analysis by Group ----

# The clean way - no brackets!
results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1],
    pooled_se = rma_fit$se[1],
    pooled_ci_lb = rma_fit$ci.lb[1],
    pooled_ci_ub = rma_fit$ci.ub[1]
  )

results
#> # A tibble: 2 × 6
#> # Rowwise:  cue_type
#>   cue_type  data           rma_fit pooled_est pooled_se pooled_ci_lb
#>   <chr>     <estmts_vc>    <rma.m>      <dbl>     <dbl>        <dbl>
#> 1 auditory  <estm_vcov>    <rma>        0.123     0.089       -0.051
#> 2 visual    <estm_vcov>    <rma>       -0.045     0.091       -0.223

# ---- Example 2: Tidy the Meta-Analysis Results ----

# Use broom::tidy() on the rma fits
tidy_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(tidy(rma_fit))

tidy_results
#> # A tibble: 2 × 7
#>   cue_type  type      estimate std.error statistic  p.value conf.low
#>   <chr>     <chr>        <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#> 1 auditory  summary      0.123    0.0889      1.38 1.67e- 1   -0.051
#> 2 visual    summary     -0.045    0.0908     -0.50 6.19e- 1   -0.223

# ---- Example 3: Glance at Model Fit Statistics ----

glance_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  ) |>
  reframe(glance(rma_fit))

glance_results
#> # A tibble: 2 × 9
#>   cue_type  nobs sigma2  tau2 df.residual  AIC  BIC logLik deviance
#>   <chr>    <int>  <dbl> <dbl>       <int> <dbl> <dbl>  <dbl>    <dbl>
#> 1 auditory     6  0.043  0           5    -8.9  -9.4    6.5      0.21
#> 2 visual       6  0.039  0           5    -9.8 -10.2    6.9      0.19

# ---- Example 4: Multiple Grouping Variables ----

results_by_country_and_cue <- ev |>
  nest_by(country, cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1],
    n_studies = nrow(data$data)
  )

results_by_country_and_cue
#> # A tibble: 6 × 5
#> # Rowwise:  country, cue_type
#>   country cue_type  data         rma_fit   pooled_est
#>   <chr>   <chr>     <estmts_vc>  <rma.mv>       <dbl>
#> 1 Canada  auditory  <estm_vcov>  <rma>          0.156
#> 2 Canada  visual    <estm_vcov>  <rma>         -0.089
#> ...

# ---- Example 5: With Moderators ----

# Run meta-regression with moderators within each cue type
moderator_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    # Country as moderator
    rma_fit = list(rma_mv_helper(data, yi = estimate, mods = ~ country))
  ) |>
  reframe(tidy(rma_fit))

moderator_results

# ---- Example 6: Extract Both Tidy and Glance ----

full_results <- ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    tidy_df = list(tidy(rma_fit)),
    glance_df = list(glance(rma_fit))
  )

# Can unnest either one
full_results |> select(cue_type, tidy_df) |> unnest(tidy_df)
full_results |> select(cue_type, glance_df) |> unnest(glance_df)

# ---- Example 7: Filter THEN Group ----

# Filter to specific countries, then analyze by cue type
ev |>
  filter(country %in% c("USA", "UK")) |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1]
  )

# ---- Example 8: Add Custom Summaries ----

ev |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    # Custom summaries
    pooled_est = rma_fit$b[1],
    pooled_se = rma_fit$se[1],
    i2 = rma_fit$I2,
    heterogeneity_p = rma_fit$QEp,
    n_effects = nrow(data$data),
    mean_raw_est = mean(data$data$estimate)
  )

# ---- Example 9: Conditional Meta-Analysis ----

# Only run meta-analysis if enough studies
conditional_results <- ev |>
  nest_by(country) |>
  mutate(
    n_studies = nrow(data$data),
    rma_fit = if (n_studies >= 3) {
      list(rma_mv_helper(data, yi = estimate))
    } else {
      list(NULL)
    },
    pooled_est = if (!is.null(rma_fit)) rma_fit$b[1] else NA_real_
  )

# ---- Example 10: Compare Fixed vs Random Effects ----

fe_vs_re <- ev |>
  nest_by(cue_type) |>
  mutate(
    fe_fit = list(rma_mv_helper(data, yi = estimate, method = "FE")),
    re_fit = list(rma_mv_helper(data, yi = estimate, method = "REML")),
    fe_est = fe_fit$b[1],
    re_est = re_fit$b[1],
    tau2 = re_fit$tau2
  )

fe_vs_re

# ========================================================================
# ALTERNATIVE: group_by() + mutate() (also clean!)
# ========================================================================

# group_by() also returns rowwise now, so no brackets needed

results_v2 <- ev |>
  group_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate)),
    pooled_est = rma_fit$b[1]
  )

results_v2

# ========================================================================
# COMPARISON: Old Ugly Way vs New Clean Way
# ========================================================================

# ❌ OLD WAY (before the fix) - UGLY BRACKETS
# ev |>
#   group_by(cue_type) |>
#   summarise(
#     rma_fit = list(rma_mv_helper(estimates_vcov[[1]], yi = estimate))
#   )

# ✅ NEW WAY - CLEAN!
ev |>
  nest_by(cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  )

# ========================================================================
# WORKFLOW SUMMARY
# ========================================================================

# The idiomatic tidyverse pattern for "regression by groups" is:
#
# 1. nest_by() - creates row-wise tibble with one row per group
# 2. mutate() - fit models on each nested data
# 3. reframe(tidy()) or reframe(glance()) - extract results
#
# This is the pattern used throughout the tidymodels ecosystem!

# Complete example:
ev |>
  nest_by(country, cue_type) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate, mods = ~ term))
  ) |>
  reframe(
    broom::tidy(rma_fit, conf.int = TRUE)
  )

# You can also keep the fit objects for later:
fitted_models <- ev |>
  nest_by(country) |>
  mutate(
    rma_fit = list(rma_mv_helper(data, yi = estimate))
  )

# Then extract what you need:
fitted_models |> reframe(tidy(rma_fit))
fitted_models |> reframe(glance(rma_fit))

# Or access individual models:
fitted_models$rma_fit[[1]]  # First country's model
summary(fitted_models$rma_fit[[1]])
