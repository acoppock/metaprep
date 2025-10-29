test_that("get_glance_df works on valid input", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("dplyr")

  library(estimatr)
  library(dplyr)

  dat <- data.frame(
    Y = rnorm(100),
    Z = factor(sample(c("T0", "T1", "T2"), 100, TRUE)),
    X = rnorm(100)
  )

  fit_1 <- lm_robust(Y ~ Z + X, data = dat)
  fit_2 <- lm_robust(Y ~ Z + X, data = dat)

  prepped_fit_1 <- prep_fit(fit_1, term = c("ZT1", "ZT2"))
  prepped_fit_2 <- prep_fit(fit_2, term = c("ZT1"))

  prepped_fits_df <- bind_rows(prepped_fit_1, prepped_fit_2, .id = "fit")

  result <- get_glance_df(prepped_fits_df)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("fit", "r.squared") %in% names(result)))
  expect_gt(nrow(result), 0)
})

test_that("get_glance_df errors on non-data-frame input", {
  expect_error(get_glance_df("not a df"),
               "must be a data frame or tibble")
})

test_that("get_glance_df errors if glance_obj missing", {
  df <- tibble::tibble(tidy_obj = list(tibble::tibble(term = "a")))
  expect_error(get_glance_df(df),
               "must contain a list-column named `glance_obj`")
})


test_that("get_glance_df drops tidy and vcov columns if present", {
  df <- tibble::tibble(
    glance_obj = list(tibble::tibble(r.squared = 0.9)),
    tidy_obj = list(tibble::tibble(term = "a")),
    vcov_obj = list(matrix(1))
  )

  result <- get_glance_df(df)
  expect_false(any(c("tidy_obj", "vcov_obj") %in% names(result)))
  expect_true("r.squared" %in% names(result))
})
