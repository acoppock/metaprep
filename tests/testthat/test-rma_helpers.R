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

  result <- quiet_uni(ev |>
    rma_uni_helper(yi = estimate, cluster = country))

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
  
  result <- quiet_uni(ev |> rma_uni_helper(yi = estimate))
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper uses diagonal of vcov by default", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- quiet_uni(ev |> rma_uni_helper(yi = estimate))
  
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
  
  result <- quiet_uni(ev |>
    rma_uni_helper(yi = estimate, mods = ~ country))
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper.list works in rowwise context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    nest_by(country) |>
    mutate(rma_fit = list(quiet_uni(rma_uni_helper(data, yi = estimate))))
  
  expect_s3_class(result$rma_fit[[1]], "rma.uni")
})

test_that("rma_uni_helper evaluates yi in data context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  # Should be able to use bare column name
  result <- quiet_uni(ev |> rma_uni_helper(yi = estimate))
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper works with filtered data", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- quiet_uni(ev |>
    filter(country == "USA") |>
    rma_uni_helper(yi = estimate))
  
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

  result <- quiet_uni(rma_uni_helper(list(ev), yi = estimate))
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
      rma_fit = list(quiet_uni(rma_uni_helper(data, yi = estimate)))
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
  result_uni <- quiet_uni(ev |> rma_uni_helper(yi = estimate))
  
  # They should both run successfully but may give different estimates
  # because rma.mv accounts for correlation
  expect_s3_class(result_mv, "rma.mv")
  expect_s3_class(result_uni, "rma.uni")
})

test_that("rma_mv_helper errors when a mods variable is absent from the object", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  # country IS on the object -> works
  expect_s3_class(
    rma_mv_helper(ev, yi = estimate, mods = ~ country, random = ~ 1 | id),
    "rma.mv"
  )
  # not_a_column is NOT on the object -> explicit error, not a silent all-FALSE
  expect_error(
    rma_mv_helper(ev, yi = estimate, mods = ~ not_a_column, random = ~ 1 | id),
    "not found in the estimates"
  )
})

test_that("rma_uni_helper errors when a mods variable is absent from the object", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  expect_error(
    rma_uni_helper(ev, yi = estimate, mods = ~ nope),
    "not found in the estimates"
  )
})

test_that("rma_mv_helper cluster = accepts a bare name, a string column, and an external vector", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("clubSandwich")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  bare <- rma_mv_helper(ev, yi = estimate, random = ~ 1 | id, cluster = country)
  strc <- rma_mv_helper(ev, yi = estimate, random = ~ 1 | id, cluster = .data[["country"]])
  vec  <- ev$estimates$country
  extv <- rma_mv_helper(ev, yi = estimate, random = ~ 1 | id, cluster = vec)

  expect_s3_class(bare, "robust.rma")
  expect_s3_class(strc, "robust.rma")
  expect_s3_class(extv, "robust.rma")
  # all three specify the same clustering, so SEs match
  expect_equal(as.numeric(bare$se), as.numeric(strc$se))
  expect_equal(as.numeric(bare$se), as.numeric(extv$se))
})

test_that("rma_mv_helper cluster = works inside a wrapper that passes a string-named column", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("clubSandwich")

  ev <- as_estimates_vcov(make_test_prepped_fits())
  pool <- function(x, cluster_var = "country") {
    rma_mv_helper(x, yi = estimate, random = ~ 1 | id, cluster = x$estimates[[cluster_var]])
  }
  expect_s3_class(pool(ev), "robust.rma")
})

# ==============================================================================
# Discarded-covariance warning and non-finite estimate guard
# ==============================================================================

test_that("rma_uni_helper warns when it discards nonzero covariances", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())
  expect_gt(sum(get_vcov(ev)[upper.tri(get_vcov(ev))] != 0), 0)

  expect_warning(
    rma_uni_helper(ev, yi = estimate),
    class = "metaprep_discarded_covariance"
  )
  expect_warning(
    rma_uni_helper(ev, yi = estimate),
    "discarding 4 nonzero covariances"
  )
})

test_that("the discarded-covariance warning names rma_mv_helper as the fix", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())
  expect_warning(rma_uni_helper(ev, yi = estimate), "rma_mv_helper")
})

test_that("an explicit vi silences the warning without changing the fit", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())

  warned <- quiet_uni(rma_uni_helper(ev, yi = estimate))
  explicit <- rma_uni_helper(ev, yi = estimate, vi = Matrix::diag(get_vcov(ev)))

  expect_no_warning(rma_uni_helper(ev, yi = estimate, vi = Matrix::diag(get_vcov(ev))))
  expect_equal(coef(warned), coef(explicit))
  expect_equal(warned$se, explicit$se)
})

test_that("an object with no covariances does not warn", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits()) |>
    filter(term == "ZT1")
  expect_equal(sum(get_vcov(ev)[upper.tri(get_vcov(ev))] != 0), 0)
  expect_no_warning(rma_uni_helper(ev, yi = estimate))
})

test_that("discarding the covariances really does shrink the standard error", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())

  mv <- rma_mv_helper(ev, yi = estimate, random = ~ 1 | id)
  uni <- quiet_uni(rma_uni_helper(ev, yi = estimate))

  # The warning is not cosmetic: this is the anticonservatism it reports.
  expect_lt(uni$se, mv$se)
})

test_that("rma_mv_helper errors on a non-finite estimate", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits()) |>
    mutate(estimate = if_else(dplyr::row_number() == 2L, NA_real_, estimate))

  expect_error(
    rma_mv_helper(ev, yi = estimate, random = ~ 1 | id),
    "cannot enter a pooled fit"
  )
  expect_error(
    rma_mv_helper(ev, yi = estimate, random = ~ 1 | id),
    "1 of 8 estimates are NA, NaN, or infinite"
  )
})

test_that("the non-finite estimate error names the affected ids", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits()) |>
    mutate(estimate = if_else(dplyr::row_number() %in% c(2L, 5L), NA_real_, estimate))

  expect_error(
    rma_mv_helper(ev, yi = estimate, random = ~ 1 | id),
    "Affected ids: 2, 5"
  )
})

test_that("rma_uni_helper errors on a non-finite estimate", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits()) |>
    mutate(estimate = if_else(dplyr::row_number() == 2L, NA_real_, estimate))

  expect_error(
    quiet_uni(rma_uni_helper(ev, yi = estimate)),
    "cannot enter a pooled fit"
  )
})

test_that("an infinite estimate is caught too", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits()) |>
    mutate(estimate = if_else(dplyr::row_number() == 1L, Inf, estimate))

  expect_error(
    rma_mv_helper(ev, yi = estimate, random = ~ 1 | id),
    "cannot enter a pooled fit"
  )
})

test_that("the finite guard fires before the covariance warning", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits()) |>
    mutate(estimate = if_else(dplyr::row_number() == 2L, NA_real_, estimate))

  # A fatal problem should not be preceded by a warning about a lesser one.
  expect_error(rma_uni_helper(ev, yi = estimate), "cannot enter a pooled fit")
})

test_that("a clean object still pools without warning or error", {
  skip_if_not_installed("metafor")
  ev <- as_estimates_vcov(make_test_prepped_fits())
  expect_no_warning(rma_mv_helper(ev, yi = estimate, random = ~ 1 | id))
  expect_no_error(rma_mv_helper(ev, yi = estimate, random = ~ 1 | id))
})

# ==============================================================================
# yi as a formula (metafor's own interface; regressed in 0.3.1, fixed in 0.4.1)
# ==============================================================================

test_that("rma_mv_helper accepts a formula for yi and fits what mods = fits", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  by_formula <- rma_mv_helper(ev, estimate ~ country, random = ~ 1 | id)
  by_mods    <- rma_mv_helper(ev, yi = estimate, mods = ~ country, random = ~ 1 | id)

  expect_s3_class(by_formula, "rma.mv")
  # The equivalence is the whole basis for passing the formula through untouched,
  # so assert it on the coefficients AND their names, not just the class.
  expect_equal(coef(by_formula), coef(by_mods))
  expect_equal(names(coef(by_formula)), names(coef(by_mods)))
  expect_equal(by_formula$se, by_mods$se)
})

test_that("an intercept-only yi formula fits what a bare yi fits", {
  skip_if_not_installed("metafor")

  # `estimate ~ 1` is the plain pooled fit written in formula form. It is the
  # most common formula call in the dependent projects, and the case a fix that
  # rebuilt the call as `mods = ~ 1` could most easily get wrong.
  ev <- as_estimates_vcov(make_test_prepped_fits())

  by_formula <- rma_mv_helper(ev, estimate ~ 1, random = ~ 1 | id)
  by_bare    <- rma_mv_helper(ev, yi = estimate, random = ~ 1 | id)

  expect_equal(coef(by_formula), coef(by_bare))
  expect_equal(by_formula$se, by_bare$se)
})

test_that("rma_uni_helper accepts a formula for yi", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  by_formula <- quiet_uni(rma_uni_helper(ev, estimate ~ country))
  by_mods    <- quiet_uni(rma_uni_helper(ev, yi = estimate, mods = ~ country))

  expect_s3_class(by_formula, "rma.uni")
  expect_equal(coef(by_formula), coef(by_mods))
})

test_that("a formula yi still runs both guards", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  # Moderator guard: the right-hand side must live on the object, exactly as it
  # must when passed as mods =. Passing the formula through whole would skip it.
  expect_error(
    rma_mv_helper(ev, estimate ~ not_a_column, random = ~ 1 | id),
    "not found in the estimates"
  )

  # Finite guard: the left-hand side is the estimate vector, so a non-finite
  # value must abort rather than be dropped silently by metafor.
  ev_na <- ev
  ev_na$estimates$estimate[1] <- NA_real_
  expect_error(
    rma_mv_helper(ev_na, estimate ~ country, random = ~ 1 | id),
    "non-finite values"
  )
})

test_that("supplying both a formula yi and mods is an error", {
  skip_if_not_installed("metafor")

  # metafor reads the moderators off the formula and never sees `mods`, so
  # accepting both would silently drop one of them.
  ev <- as_estimates_vcov(make_test_prepped_fits())

  expect_error(
    rma_mv_helper(ev, estimate ~ country, mods = ~ study_type, random = ~ 1 | id),
    "also supplied"
  )
  expect_error(
    quiet_uni(rma_uni_helper(ev, estimate ~ country, mods = ~ study_type)),
    "also supplied"
  )
})

test_that("a one-sided yi formula names no estimates and errors", {
  skip_if_not_installed("metafor")

  ev <- as_estimates_vcov(make_test_prepped_fits())

  expect_error(
    rma_mv_helper(ev, ~ country, random = ~ 1 | id),
    "one-sided formula"
  )
})

test_that("a formula yi survives rowwise list dispatch", {
  skip_if_not_installed("metafor")

  # The dependent projects reach the helper through nest_by() + mutate(), which
  # dispatches on .list and forwards `yi` as an expression. The formula has to
  # survive that hop, not just the direct call.
  ev <- as_estimates_vcov(make_test_prepped_fits())

  by_formula <- ev |>
    dplyr::nest_by(country) |>
    dplyr::mutate(fit = list(rma_mv_helper(data, estimate ~ 1, random = ~ 1 | id)))
  by_bare <- ev |>
    dplyr::nest_by(country) |>
    dplyr::mutate(fit = list(rma_mv_helper(data, yi = estimate, random = ~ 1 | id)))

  expect_s3_class(by_formula$fit[[1]], "rma.mv")
  expect_equal(coef(by_formula$fit[[1]]), coef(by_bare$fit[[1]]))
})
