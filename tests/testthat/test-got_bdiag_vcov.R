test_that("get_bdiag_vcov works with valid input", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("estimatr")
  skip_if_not_installed("dplyr")

  library(estimatr)
  library(dplyr)

  dat <- data.frame(
    Y = rnorm(50),
    Z = factor(sample(c("T0", "T1", "T2"), 50, TRUE)),
    X = rnorm(50)
  )

  fit_1 <- lm_robust(Y ~ Z + X, data = dat)
  fit_2 <- lm_robust(Y ~ Z + X, data = dat)

  prepped_fit_1 <- prep_fit(fit_1, term = c("ZT1", "ZT2"))
  prepped_fit_2 <- prep_fit(fit_2, term = c("ZT1"))

  prepped_fits_df <- bind_rows(prepped_fit_1, prepped_fit_2, .id = "fit")

  result <- get_bdiag_vcov(prepped_fits_df)

  expect_s4_class(result, "dsCMatrix")
})

test_that("get_bdiag_vcov errors on non-data-frame input", {
  expect_error(get_bdiag_vcov("not a df"),
               "must be a data frame or tibble")
})

test_that("get_bdiag_vcov errors when vcov_obj missing", {
  fake_df <- tibble::tibble(glance_obj = list(tibble::tibble(a = 1)))
  expect_error(get_bdiag_vcov(fake_df),
               "must contain a list-column named `vcov_obj`")
})


test_that("get_bdiag_vcov combines multiple matrices correctly", {
  m1 <- diag(2)
  m2 <- diag(3)
  df <- tibble::tibble(vcov_obj = list(m1, m2))

  result <- get_bdiag_vcov(df)
  expect_equal(dim(result), c(5, 5))
})
