# Tests for dplyr methods on estimates_vcov objects
# tests/testthat/test-estimates_vcov_dplyr.R

library(testthat)
library(metaprep)
library(estimatr)
library(dplyr)

# ---- Test Fixtures ----

make_test_data <- function(n = 100) {
  data.frame(
    Y = rnorm(n),
    Z = factor(sample(c("T0", "T1", "T2"), n, TRUE)),
    country = sample(c("USA", "UK"), n, TRUE),
    study_type = sample(c("RCT", "observational"), n, TRUE)
  )
}

make_test_prepped_fits <- function() {
  dat <- make_test_data()
  
  fits <- dat |>
    nest_by(country, study_type) |>
    mutate(
      fit_obj = list(lm_robust(Y ~ Z, data = data)),
      prep_obj = list(prep_fit(fit_obj, term = c("ZT1", "ZT2")))
    ) |>
    unnest(prep_obj) |>
    ungroup()
  
  fits
}

# ==============================================================================
# Filter Tests
# ==============================================================================

test_that("filter maintains vcov synchronization", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(country == "USA")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_equal(nrow(filtered$estimates), nrow(filtered$vcov))
  expect_equal(nrow(filtered$vcov), ncol(filtered$vcov))
  expect_true(all(filtered$estimates$country == "USA"))
})

test_that("filter with multiple conditions works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(country == "USA", term == "ZT1")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_true(all(filtered$estimates$country == "USA"))
  expect_true(all(filtered$estimates$term == "ZT1"))
})

test_that("filter to zero rows works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(country == "Nonexistent")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_equal(nrow(filtered$estimates), 0)
  expect_equal(dim(filtered$vcov), c(0, 0))
})

# ==============================================================================
# Slice Tests
# ==============================================================================

test_that("slice maintains vcov synchronization", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  sliced <- ev |> slice(1:3)
  
  expect_s3_class(sliced, "estimates_vcov")
  expect_equal(nrow(sliced$estimates), 3)
  expect_equal(nrow(sliced$vcov), 3)
  expect_equal(ncol(sliced$vcov), 3)
})

test_that("slice_head works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  sliced <- ev |> slice_head(n = 2)
  
  expect_equal(nrow(sliced$estimates), 2)
  expect_equal(nrow(sliced$vcov), 2)
})

test_that("slice_tail works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  n_rows <- nrow(ev$estimates)
  
  sliced <- ev |> slice_tail(n = 2)
  
  expect_equal(nrow(sliced$estimates), 2)
  expect_equal(nrow(sliced$vcov), 2)
})

# ==============================================================================
# Arrange Tests
# ==============================================================================

test_that("arrange maintains vcov synchronization", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  original_order <- ev$estimates$estimate
  
  arranged <- ev |> arrange(desc(estimate))
  
  expect_s3_class(arranged, "estimates_vcov")
  expect_equal(nrow(arranged$estimates), nrow(ev$estimates))
  expect_equal(nrow(arranged$vcov), nrow(ev$vcov))
  expect_equal(arranged$estimates$estimate, sort(original_order, decreasing = TRUE))
})

test_that("arrange by multiple columns works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  arranged <- ev |> arrange(country, desc(estimate))
  
  expect_s3_class(arranged, "estimates_vcov")
  expect_equal(nrow(arranged$estimates), nrow(ev$estimates))
})

# ==============================================================================
# Mutate Tests
# ==============================================================================

test_that("mutate adds columns without changing vcov", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  original_vcov <- ev$vcov
  
  mutated <- ev |> mutate(abs_estimate = abs(estimate))
  
  expect_s3_class(mutated, "estimates_vcov")
  expect_true("abs_estimate" %in% names(mutated$estimates))
  expect_identical(mutated$vcov, original_vcov)
})

test_that("mutate can modify existing columns", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  mutated <- ev |> mutate(estimate = estimate * 2)
  
  expect_equal(mutated$estimates$estimate, ev$estimates$estimate * 2)
  expect_identical(mutated$vcov, ev$vcov)
})

test_that("mutate can create multiple columns", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  mutated <- ev |> mutate(
    abs_est = abs(estimate),
    se2 = std.error^2
  )
  
  expect_true(all(c("abs_est", "se2") %in% names(mutated$estimates)))
})

# ==============================================================================
# Select Tests
# ==============================================================================

test_that("select keeps vcov unchanged", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  selected <- ev |> select(id, term, estimate, std.error)
  
  expect_s3_class(selected, "estimates_vcov")
  expect_equal(ncol(selected$estimates), 4)
  expect_identical(selected$vcov, ev$vcov)
})

test_that("select can use helpers", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  selected <- ev |> select(starts_with("est"))
  
  expect_s3_class(selected, "estimates_vcov")
  expect_true("estimate" %in% names(selected$estimates))
})

# ==============================================================================
# Rename Tests
# ==============================================================================

test_that("rename keeps vcov unchanged", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  renamed <- ev |> rename(effect = estimate)
  
  expect_s3_class(renamed, "estimates_vcov")
  expect_true("effect" %in% names(renamed$estimates))
  expect_false("estimate" %in% names(renamed$estimates))
  expect_identical(renamed$vcov, ev$vcov)
})

test_that("rename multiple columns works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  renamed <- ev |> rename(effect = estimate, se = std.error)
  
  expect_true(all(c("effect", "se") %in% names(renamed$estimates)))
  expect_false(any(c("estimate", "std.error") %in% names(renamed$estimates)))
})

# ==============================================================================
# Relocate Tests
# ==============================================================================

test_that("relocate keeps vcov unchanged", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  relocated <- ev |> relocate(country, .before = term)
  
  expect_s3_class(relocated, "estimates_vcov")
  expect_identical(relocated$vcov, ev$vcov)
})

test_that("relocate changes column order", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  relocated <- ev |> relocate(estimate, .before = id)
  
  expect_equal(names(relocated$estimates)[1], "estimate")
})

# ==============================================================================
# Pull Tests
# ==============================================================================

test_that("pull returns vector not estimates_vcov", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  estimates <- ev |> pull(estimate)
  
  expect_type(estimates, "double")
  expect_equal(length(estimates), nrow(ev$estimates))
  expect_false(inherits(estimates, "estimates_vcov"))
})

test_that("pull with name argument works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  estimates <- ev |> pull(estimate, name = term)
  
  expect_true(!is.null(names(estimates)))
})

# ==============================================================================
# nest_by() Tests
# ==============================================================================

test_that("nest_by creates rowwise tibble with estimates_vcov objects", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  nested <- ev |> nest_by(country)
  
  expect_s3_class(nested, "rowwise_df")
  expect_true("data" %in% names(nested))
  expect_s3_class(nested$data[[1]], "estimates_vcov")
})

test_that("nest_by respects .key argument", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  nested <- ev |> nest_by(country, .key = "ev_obj")
  
  expect_true("ev_obj" %in% names(nested))
  expect_false("data" %in% names(nested))
  expect_s3_class(nested$ev_obj[[1]], "estimates_vcov")
})

test_that("nest_by maintains vcov within groups", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  nested <- ev |> nest_by(country)
  
  for (i in seq_len(nrow(nested))) {
    ev_group <- nested$data[[i]]
    expect_s3_class(ev_group, "estimates_vcov")
    expect_equal(nrow(ev_group$estimates), nrow(ev_group$vcov))
    expect_equal(nrow(ev_group$vcov), ncol(ev_group$vcov))
  }
})

test_that("nest_by with multiple grouping variables works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  nested <- ev |> nest_by(country, study_type)
  
  expect_s3_class(nested, "rowwise_df")
  expect_true(all(c("country", "study_type", "data") %in% names(nested)))
})

# ==============================================================================
# Join Tests
# ==============================================================================

test_that("left_join adds columns without changing rows", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  lookup <- data.frame(
    country = c("USA", "UK"),
    region = c("North America", "Europe")
  )
  
  joined <- ev |> left_join(lookup, by = "country")
  
  expect_s3_class(joined, "estimates_vcov")
  expect_true("region" %in% names(joined$estimates))
  expect_equal(nrow(joined$estimates), nrow(ev$estimates))
  expect_identical(joined$vcov, ev$vcov)
})

test_that("left_join errors when rows would change", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  # Duplicate key would create more rows
  lookup <- data.frame(
    country = c("USA", "USA", "UK"),
    region = c("West", "East", "Europe")
  )
  
  expect_error(
    ev |> left_join(lookup, by = "country"),
    "changed the number of rows"
  )
})

test_that("right_join is rejected", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  lookup <- data.frame(country = "USA")
  
  expect_error(
    ev |> right_join(lookup, by = "country"),
    "not supported"
  )
})

test_that("inner_join is rejected", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  lookup <- data.frame(country = "USA")
  
  expect_error(
    ev |> inner_join(lookup, by = "country"),
    "not supported"
  )
})

test_that("full_join is rejected", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  lookup <- data.frame(country = c("USA", "UK", "Germany"))
  
  expect_error(
    ev |> full_join(lookup, by = "country"),
    "not supported"
  )
})

test_that("semi_join filters rows correctly", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filter_df <- data.frame(country = "USA")
  
  filtered <- ev |> semi_join(filter_df, by = "country")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_true(all(filtered$estimates$country == "USA"))
  expect_equal(nrow(filtered$estimates), nrow(filtered$vcov))
})

test_that("anti_join filters rows correctly", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filter_df <- data.frame(country = "USA")
  
  filtered <- ev |> anti_join(filter_df, by = "country")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_true(all(filtered$estimates$country != "USA"))
  expect_equal(nrow(filtered$estimates), nrow(filtered$vcov))
})

# ==============================================================================
# Chaining and Edge Cases
# ==============================================================================

test_that("chaining multiple operations works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    filter(country == "USA") |>
    mutate(abs_est = abs(estimate)) |>
    arrange(desc(abs_est)) |>
    select(id, term, abs_est, std.error)
  
  expect_s3_class(result, "estimates_vcov")
  expect_true("abs_est" %in% names(result$estimates))
  expect_equal(ncol(result$estimates), 4)
})

test_that("id column persists through operations", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(term == "ZT1")
  mutated <- ev |> mutate(new_col = 1)
  arranged <- ev |> arrange(estimate)
  
  expect_true("id" %in% names(filtered$estimates))
  expect_true("id" %in% names(mutated$estimates))
  expect_true("id" %in% names(arranged$estimates))
})

test_that("rownames and colnames of vcov match id after operations", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(country == "USA")
  arranged <- ev |> arrange(estimate)
  sliced <- ev |> slice(1:5)
  
  expect_equal(rownames(filtered$vcov), filtered$estimates$id)
  expect_equal(rownames(arranged$vcov), arranged$estimates$id)
  expect_equal(rownames(sliced$vcov), sliced$estimates$id)
})

test_that("empty filter preserves structure", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  empty <- ev |> filter(estimate > 999)
  
  expect_s3_class(empty, "estimates_vcov")
  expect_equal(nrow(empty$estimates), 0)
  expect_equal(dim(empty$vcov), c(0, 0))
})
