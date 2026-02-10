# Test Suite for estimates_vcov Class
# tests/testthat/test-estimates-vcov.R

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
    cue_type = sample(c("visual", "auditory"), n, TRUE)
  )
}

make_test_prepped_fits <- function() {
  dat <- make_test_data()
  
  fits <- dat |>
    nest_by(country, cue_type) |>
    mutate(
      fit_obj = list(lm_robust(Y ~ Z, data = data)),
      prep_obj = list(prep_fit(fit_obj, term = c("ZT1", "ZT2")))
    ) |>
    unnest(prep_obj) |>
    ungroup()
  
  fits
}

# ---- Test Construction ----

test_that("as_estimates_vcov creates valid object", {
  prepped_fits <- make_test_prepped_fits()
  ev <- as_estimates_vcov(prepped_fits)
  
  expect_s3_class(ev, "estimates_vcov")
  expect_true(is.list(ev))
  expect_named(ev, c("data", "vcov", "row_map"))
  expect_s3_class(ev$data, "tbl_df")
  expect_true(is.matrix(ev$vcov))
})

test_that("vcov dimensions match data rows", {
  prepped_fits <- make_test_prepped_fits()
  ev <- as_estimates_vcov(prepped_fits)
  
  expect_equal(nrow(ev$data), nrow(ev$vcov))
  expect_equal(nrow(ev$vcov), ncol(ev$vcov))
})

test_that("as_estimates_vcov validates input", {
  expect_error(
    as_estimates_vcov(mtcars),
    "tidy_obj.*vcov_obj"
  )
  
  expect_error(
    as_estimates_vcov("not a data frame"),
    "must be a data frame"
  )
})

test_that("as_estimates_vcov requires _obj naming", {
  prepped_fits <- make_test_prepped_fits()
  
  # Rename to wrong names
  wrong_names <- prepped_fits |>
    rename(tidy = tidy_obj, vcov = vcov_obj)
  
  expect_error(
    as_estimates_vcov(wrong_names),
    "tidy_obj.*vcov_obj"
  )
})

# ---- Test Print Method ----

test_that("print method works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  expect_output(print(ev), "estimates_vcov")
  expect_output(print(ev), "estimates with.*vcov matrix")
})

# ---- Test Accessors ----

test_that("get_data extracts data", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  data <- get_data(ev)
  
  expect_s3_class(data, "tbl_df")
  expect_identical(data, ev$data)
})

test_that("get_vcov extracts vcov", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  vcov <- get_vcov(ev)
  
  expect_true(is.matrix(vcov))
  expect_identical(vcov, ev$vcov)
})

test_that("as_tibble converts to tibble", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  tb <- as_tibble(ev)
  
  expect_s3_class(tb, "tbl_df")
  expect_identical(tb, ev$data)
})

# ---- Test Filter ----

test_that("filter maintains vcov synchronization", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(country == "USA")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_equal(nrow(filtered$data), nrow(filtered$vcov))
  expect_equal(nrow(filtered$vcov), ncol(filtered$vcov))
  expect_true(all(filtered$data$country == "USA"))
})

test_that("filter handles multiple conditions", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(country == "USA", cue_type == "visual")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_true(all(filtered$data$country == "USA"))
  expect_true(all(filtered$data$cue_type == "visual"))
})

test_that("filter to zero rows works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  filtered <- ev |> filter(country == "Nonexistent")
  
  expect_s3_class(filtered, "estimates_vcov")
  expect_equal(nrow(filtered$data), 0)
  expect_equal(nrow(filtered$vcov), 0)
})

# ---- Test Slice ----

test_that("slice maintains vcov synchronization", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  sliced <- ev |> slice(1:3)
  
  expect_s3_class(sliced, "estimates_vcov")
  expect_equal(nrow(sliced$data), 3)
  expect_equal(nrow(sliced$vcov), 3)
  expect_equal(ncol(sliced$vcov), 3)
})

test_that("slice_head works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  sliced <- ev |> slice_head(n = 2)
  
  expect_equal(nrow(sliced$data), 2)
  expect_equal(nrow(sliced$vcov), 2)
})

# ---- Test Arrange ----

test_that("arrange maintains vcov synchronization", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  original_order <- ev$data$estimate
  
  arranged <- ev |> arrange(desc(estimate))
  
  expect_s3_class(arranged, "estimates_vcov")
  expect_equal(nrow(arranged$data), nrow(ev$data))
  expect_equal(nrow(arranged$vcov), nrow(ev$vcov))
  expect_false(identical(arranged$data$estimate, original_order))
  expect_equal(arranged$data$estimate, sort(original_order, decreasing = TRUE))
})

# ---- Test Mutate ----

test_that("mutate adds columns without changing vcov", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  mutated <- ev |> mutate(abs_estimate = abs(estimate))
  
  expect_s3_class(mutated, "estimates_vcov")
  expect_true("abs_estimate" %in% names(mutated$data))
  expect_identical(mutated$vcov, ev$vcov)
})

test_that("mutate can modify existing columns", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  mutated <- ev |> mutate(estimate = estimate * 2)
  
  expect_equal(mutated$data$estimate, ev$data$estimate * 2)
  expect_identical(mutated$vcov, ev$vcov)
})

# ---- Test Select ----

test_that("select keeps vcov unchanged", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  selected <- ev |> select(term, estimate, std.error)
  
  expect_s3_class(selected, "estimates_vcov")
  expect_equal(ncol(selected$data), 3)
  expect_identical(selected$vcov, ev$vcov)
})

# ---- Test Rename ----

test_that("rename keeps vcov unchanged", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  renamed <- ev |> rename(effect = estimate)
  
  expect_s3_class(renamed, "estimates_vcov")
  expect_true("effect" %in% names(renamed$data))
  expect_false("estimate" %in% names(renamed$data))
  expect_identical(renamed$vcov, ev$vcov)
})

# ---- Test Relocate ----

test_that("relocate keeps vcov unchanged", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  relocated <- ev |> relocate(country, .before = term)
  
  expect_s3_class(relocated, "estimates_vcov")
  expect_identical(relocated$vcov, ev$vcov)
  expect_equal(names(relocated$data)[1], "country")
})

# ---- Test Pull ----

test_that("pull returns vector", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  estimates <- ev |> pull(estimate)
  
  expect_type(estimates, "double")
  expect_equal(length(estimates), nrow(ev$data))
})

# ---- Test Nest By ----

test_that("nest_by creates rowwise tibble", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  nested <- ev |> nest_by(country)
  
  expect_s3_class(nested, "tbl_df")
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

test_that("nest_by works with mutate (clean syntax)", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    nest_by(cue_type) |>
    mutate(
      rma_fit = list(rma_mv_helper(data, yi = estimate)),
      pooled_est = rma_fit$b[1]
    )
  
  expect_s3_class(result, "tbl_df")
  expect_true("pooled_est" %in% names(result))
  expect_type(result$pooled_est, "double")
})

test_that("nest_by maintains vcov within groups", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  nested <- ev |> nest_by(country)
  
  for (i in seq_len(nrow(nested))) {
    ev_group <- nested$data[[i]]
    expect_equal(nrow(ev_group$data), nrow(ev_group$vcov))
    expect_equal(nrow(ev_group$vcov), ncol(ev_group$vcov))
  }
})

# ---- Test Chaining ----

test_that("multiple operations can be chained", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    filter(country == "USA") |>
    mutate(abs_est = abs(estimate)) |>
    arrange(desc(abs_est)) |>
    slice_head(n = 2)
  
  expect_s3_class(result, "estimates_vcov")
  expect_equal(nrow(result$data), 2)
  expect_equal(nrow(result$vcov), 2)
})

# ---- Test RMA Helpers ----

test_that("rma_mv_helper works", {
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
    rma_mv_helper(yi = estimate)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper works with moderators", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    rma_mv_helper(yi = estimate, mods = ~ cue_type)
  
  expect_s3_class(result, "rma.mv")
})

test_that("rma_mv_helper.list works in rowwise context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    nest_by(country) |>
    mutate(
      rma_fit = list(rma_mv_helper(data, yi = estimate))
    )
  
  expect_s3_class(result$rma_fit[[1]], "rma.mv")
})

test_that("rma_uni_helper works", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |> rma_uni_helper(yi = estimate)
  
  expect_s3_class(result, "rma.uni")
})

test_that("rma_uni_helper.list works in rowwise context", {
  skip_if_not_installed("metafor")
  
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  result <- ev |>
    nest_by(country) |>
    mutate(
      rma_fit = list(rma_uni_helper(data, yi = estimate))
    )
  
  expect_s3_class(result$rma_fit[[1]], "rma.uni")
})

# ---- Test fix_vcov ----

test_that("fix_vcov symmetrizes matrix", {
  # Create asymmetric matrix
  m <- matrix(c(1, 0.1, 0.2, 1), 2, 2)
  
  fixed <- fix_vcov(m, method = "symmetrize")
  
  expect_equal(fixed, t(fixed))
  expect_equal(fixed[1, 2], fixed[2, 1])
})

test_that("fix_vcov handles negative eigenvalues", {
  # Create matrix with small negative eigenvalue
  m <- matrix(c(1, 0.5, 0.5, 0.1), 2, 2)
  eigs_before <- eigen(m)$values
  
  fixed <- fix_vcov(m, method = "near_psd")
  eigs_after <- eigen(fixed)$values
  
  expect_true(all(eigs_after >= 0))
})

test_that("fix_vcov works on estimates_vcov objects", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  fixed_ev <- fix_vcov(ev, method = "both")
  
  expect_s3_class(fixed_ev, "estimates_vcov")
  expect_equal(nrow(fixed_ev$data), nrow(ev$data))
  expect_equal(fixed_ev$vcov, t(fixed_ev$vcov))
})

# ---- Test Edge Cases ----

test_that("single row estimates_vcov works", {
  prepped_fits <- make_test_prepped_fits()
  single_row <- prepped_fits[1, ]
  
  ev <- as_estimates_vcov(single_row)
  
  expect_equal(nrow(ev$data), nrow(single_row$tidy_obj[[1]]))
  expect_equal(nrow(ev$vcov), nrow(ev$data))
})

test_that("empty filter preserves structure", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  empty <- ev |> filter(estimate > 999)
  
  expect_s3_class(empty, "estimates_vcov")
  expect_equal(nrow(empty$data), 0)
  expect_equal(dim(empty$vcov), c(0, 0))
})

# ---- Test Extraction Functions ----

test_that("get_estimates_df works", {
  prepped_fits <- make_test_prepped_fits()
  
  estimates_df <- get_estimates_df(prepped_fits)
  
  expect_s3_class(estimates_df, "tbl_df")
  expect_false("vcov_obj" %in% names(estimates_df))
})

test_that("get_glance_df works", {
  prepped_fits <- make_test_prepped_fits()
  
  glance_df <- get_glance_df(prepped_fits)
  
  expect_s3_class(glance_df, "tbl_df")
  expect_false("tidy_obj" %in% names(glance_df))
})

test_that("get_bdiag_vcov works", {
  prepped_fits <- make_test_prepped_fits()
  
  vcov_bdiag <- get_bdiag_vcov(prepped_fits)
  
  expect_true(inherits(vcov_bdiag, "dgCMatrix") || is.matrix(vcov_bdiag))
})

# ---- Test prep_fit ----

test_that("prep_fit returns correct columns", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = "ZT1")
  
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(prepped)))
})

test_that("prep_fit filters terms correctly", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = "ZT1")
  
  expect_true(all(str_detect(prepped$tidy_obj[[1]]$term, "ZT1")))
})
