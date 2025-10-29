test_that("get_estimates_df works on valid input", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("broom")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  library(estimatr)
  library(dplyr)

  # Example data
  dat <- data.frame(
    Y = rnorm(100),
    Z = factor(sample(c("T0", "T1", "T2"), 100, TRUE)),
    X = rnorm(100)
  )

  # Fit two models
  fit_1 <- lm_robust(Y ~ Z + X, data = dat)
  fit_2 <- lm_robust(Y ~ Z + X, data = dat)

  # Prep them
  prepped_fit_1 <- prep_fit(fit_1, term = c("ZT1", "ZT2"))
  prepped_fit_2 <- prep_fit(fit_2, term = c("ZT1"))

  prepped_fits_df <- bind_rows(prepped_fit_1, prepped_fit_2, .id = "fit")

  # Run
  result <- get_estimates_df(prepped_fits_df)

  # Structure checks
  expect_s3_class(result, "data.frame")
  expect_true(all(c("fit", "estimate", "term") %in% names(result)))
  expect_false(any(c("glance_obj", "vcov_obj") %in% names(result)))
  expect_gt(nrow(result), 0)
})

test_that("get_estimates_df errors on non-data-frame input", {
  expect_error(get_estimates_df("not a df"),
               "must be a data frame or tibble")
})

test_that("get_estimates_df errors if tidy_obj missing", {
  fake_df <- tibble::tibble(glance_obj = list(tibble::tibble(a = 1)))
  expect_error(get_estimates_df(fake_df),
               "must contain a list-column named `tidy_obj`")
})

test_that("get_estimates_df handles missing optional columns gracefully", {
  # A minimal valid example
  df <- tibble::tibble(
    tidy_obj = list(tibble::tibble(term = "a", estimate = 1))
  )

  result <- get_estimates_df(df)
  expect_equal(names(result), c("term", "estimate"))
})

test_that("get_estimates_df drops glance/vcov columns if present", {
  df <- tibble::tibble(
    tidy_obj = list(tibble::tibble(term = "a", estimate = 1)),
    glance_obj = list(tibble::tibble(r.squared = 0.9)),
    vcov_obj = list(matrix(1))
  )

  result <- get_estimates_df(df)
  expect_false(any(c("glance_obj", "vcov_obj") %in% names(result)))
})
