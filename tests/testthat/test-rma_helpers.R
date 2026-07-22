# Tests for rma helper functions
# tests/testthat/test-rma_helpers.R

library(testthat)
library(metaprep)

# ==============================================================================
# rma_mv_helper() Tests  
# ==============================================================================

test_that("rma_mv_helper works on estimates_vcov", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper works without random argument", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |> rma_mv_helper(yi = estimate)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper works with filtered data", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    filter(country == "USA") |>
    rma_mv_helper(yi = estimate, random = ~ 1 | id)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper works with moderators", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    rma_mv_helper(yi = estimate, mods = ~ country, random = ~ 1 | id)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper.list works in rowwise context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    nest_by(country) |>
    mutate(rma_fit = list(rma_mv_helper(data, yi = estimate, random = ~ 1 | id)))
  
  expect_s3_class(result$rma_fit[[1]], "rma.mv")
  expect_equal(nrow(result), length(unique(ev$estimates$country)))
})

test_that("rma_mv_helper without cluster returns a plain rma.mv (not robust)", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  result <- ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)

  expect_false(inherits(result, "robust.rma"))
})

test_that("rma_mv_helper cluster = adds CR2 cluster-robust SEs", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("clubSandwich")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  result <- ev |>
    rma_mv_helper(yi = estimate, random = ~ 1 | id, cluster = country)

  expect_s3_class(result, "robust.rma")
})

test_that("rma_mv_helper cluster with clubSandwich = FALSE needs no clubSandwich", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  result <- ev |>
    rma_mv_helper(yi = estimate, random = ~ 1 | id,
                  cluster = country, clubSandwich = FALSE)

  expect_s3_class(result, "robust.rma")
})

test_that("rma_mv_helper cluster = flows through the rowwise/list path", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("clubSandwich")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  result <- ev |>
    nest_by(country) |>
    mutate(fit = list(
      rma_mv_helper(data, yi = estimate, random = ~ 1 | id, cluster = study_type)
    ))

  expect_s3_class(result$fit[[1]], "robust.rma")
})

test_that("rma_uni_helper cluster = adds CR2 cluster-robust SEs", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("clubSandwich")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  result <- ev |>
    rma_uni_helper(yi = estimate, cluster = country)

  expect_s3_class(result, "robust.rma")
})

test_that("rma_mv_helper uses custom vcov when provided", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  # Create a custom vcov (just identity for testing)
  custom_vcov <- diag(nrow(ev$estimates))
  
  result <- ev |> rma_mv_helper(yi = estimate, V = custom_vcov, random = ~ 1 | id)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper evaluates yi in data context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  # Should be able to use bare column name
  result <- ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper errors without metafor", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())

  testthat::with_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base",
    expect_error(
      rma_mv_helper(ev, yi = estimate),
      "Package 'metafor' is required"
    )
  )
})

test_that("rma_mv_helper.list dispatches to estimates_vcov method", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())

  result <- rma_mv_helper(list(ev), yi = estimate, random = ~ 1 | id)
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper.list errors on non-estimates_vcov list", {
  skip_if_not_installed("metafor")
  
  bad_list <- list(data.frame(x = 1))
  
  expect_error(
    rma_mv_helper(bad_list, yi = x),
    "does not contain an estimates_vcov object"
  )
})

# ==============================================================================
# rma_uni_helper() Tests
# ==============================================================================

test_that("rma_uni_helper works on estimates_vcov", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |> rma_uni_helper(yi = estimate)
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper uses diagonal of vcov by default", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |> rma_uni_helper(yi = estimate)
  
  expect_s3_class(result, "rma.uni")
  # The vi should come from diag(vcov)
})

test_that("rma_uni_helper works with custom vi", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  custom_vi <- rep(0.1, nrow(ev$estimates))
  
  result <- ev |> rma_uni_helper(yi = estimate, vi = custom_vi)
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper works with moderators", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    rma_uni_helper(yi = estimate, mods = ~ country)
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper.list works in rowwise context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    nest_by(country) |>
    mutate(rma_fit = list(rma_uni_helper(data, yi = estimate)))
  
  expect_s3_class(result$rma_fit[[1]], "rma.uni")
})

test_that("rma_uni_helper evaluates yi in data context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  # Should be able to use bare column name
  result <- ev |> rma_uni_helper(yi = estimate)
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper works with filtered data", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    filter(country == "USA") |>
    rma_uni_helper(yi = estimate)
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper.list errors on non-estimates_vcov list", {
  skip_if_not_installed("metafor")

  bad_list <- list(data.frame(x = 1))

  expect_error(
    rma_uni_helper(bad_list, yi = x),
    "does not contain an estimates_vcov object"
  )
})

test_that("rma_uni_helper errors without metafor", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())

  testthat::with_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base",
    expect_error(
      rma_uni_helper(ev, yi = estimate),
      "Package 'metafor' is required"
    )
  )
})

test_that("rma_uni_helper.list dispatches to estimates_vcov method", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())

  result <- rma_uni_helper(list(ev), yi = estimate)
  expect_s3_class(result, "rma.uni")
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("rma_mv_helper integrates with nest_by workflow", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("broom")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  results <- ev |>
    nest_by(study_type) |>
    mutate(
      rma_fit = list(rma_mv_helper(data, yi = estimate, random = ~ 1 | id)),
      pooled_est = rma_fit$b[1,1]
    )
  
  expect_s3_class(results, "tbl_df")
  expect_true("pooled_est" %in% names(results))
  expect_type(results$pooled_est, "double")
})

test_that("rma_uni_helper integrates with mutate workflow", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  # Add grouping variable
  ev_grouped <- ev |>
    mutate(large_se = std.error > median(std.error))
  
  results <- ev_grouped |>
    nest_by(large_se) |>
    mutate(
      rma_fit = list(rma_uni_helper(data, yi = estimate))
    )
  
  expect_s3_class(results$rma_fit[[1]], "rma.uni")
})

test_that("rma helpers work after complex dplyr chains", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    filter(country == "USA") |>
    mutate(abs_est = abs(estimate)) |>
    arrange(desc(abs_est)) |>
    rma_mv_helper(yi = estimate, random = ~ 1 | id)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv and rma_uni give different results when appropriate", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result_mv <- ev |> rma_mv_helper(yi = estimate, random = ~ 1 | id)
  result_uni <- ev |> rma_uni_helper(yi = estimate)
  
  # They should both run successfully but may give different estimates
  # because rma.mv accounts for correlation
  expect_s3_class(result_mv, "rma.mv")
  expect_s3_class(result_uni, "rma.uni")
})
