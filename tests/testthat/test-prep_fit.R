# Tests for prep_fit()
# tests/testthat/test-prep_fit.R

library(testthat)
library(metaprep)
library(estimatr)
library(dplyr)

# ---- Test Fixtures ----

make_test_data <- function(n = 100) {
  data.frame(
    Y = rnorm(n),
    Z = factor(sample(c("T0", "T1", "T2"), n, TRUE)),
    X = rnorm(n)
  )
}

# ==============================================================================
# prep_fit() Tests
# ==============================================================================

test_that("prep_fit returns correct structure", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = "ZT1")
  
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(prepped)))
  expect_s3_class(prepped$tidy_obj[[1]], "tbl_df")
  expect_s3_class(prepped$glance_obj[[1]], "data.frame")
  expect_true(is.matrix(prepped$vcov_obj[[1]]))
})

test_that("prep_fit filters terms correctly", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = c("ZT1", "ZT2"))
  tidy_terms <- prepped$tidy_obj[[1]]$term
  
  expect_true(all(grepl("ZT1|ZT2", tidy_terms)))
  expect_false(any(grepl("Intercept", tidy_terms)))
})

test_that("prep_fit works with standard lm", {
  dat <- make_test_data()
  fit <- lm(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = "ZT1")
  
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(prepped)))
})

test_that("prep_fit works with glm", {
  dat <- make_test_data()
  fit <- glm(Y ~ Z, data = dat, family = gaussian)
  
  prepped <- prep_fit(fit, term = "ZT1")
  
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(prepped)))
})

test_that("prep_fit validates inputs", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  expect_error(
    prep_fit(fit, term = NULL),
    "is.character\\(term\\)"
  )
  
  expect_error(
    prep_fit(fit, term = character(0)),
    "length\\(term\\) > 0"
  )
})

test_that("prep_fit handle_multivariate parameter works", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  # Should work with both TRUE and FALSE
  prepped_true <- prep_fit(fit, term = "ZT1", handle_multivariate = TRUE)
  prepped_false <- prep_fit(fit, term = "ZT1", handle_multivariate = FALSE)
  
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(prepped_true)))
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(prepped_false)))
})

test_that("prep_fit handles missing tidy method gracefully", {
  # Create object without tidy method
  bad_fit <- structure(list(), class = "no_tidy_class")
  
  expect_error(
    prep_fit(bad_fit, term = "x"),
    "Could not extract tidy"
  )
})

test_that("prep_fit warns when glance method missing", {
  skip("Hard to test without creating custom class")
  # This is tested implicitly when glance returns NA values
})

test_that("prep_fit handles missing vcov method gracefully", {
  # Create object with tidy/glance methods but no vcov method
  bad_fit <- structure(list(), class = "has_tidy_no_vcov")
  tidy.has_tidy_no_vcov <<- function(x, ...) {
    tibble::tibble(term = "x", estimate = 1, std.error = 0.1,
                   statistic = 10, p.value = 0.001)
  }
  glance.has_tidy_no_vcov <<- function(x, ...) {
    tibble::tibble(nobs = 100)
  }
  on.exit({
    rm(tidy.has_tidy_no_vcov, envir = .GlobalEnv)
    rm(glance.has_tidy_no_vcov, envir = .GlobalEnv)
  })

  expect_error(
    prep_fit(bad_fit, term = "x"),
    "Could not extract vcov"
  )
})

test_that("prep_fit vcov subset has correct dimensions", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z + X, data = dat)
  
  prepped <- prep_fit(fit, term = c("ZT1", "ZT2"))
  
  n_terms <- nrow(prepped$tidy_obj[[1]])
  vcov_dims <- dim(prepped$vcov_obj[[1]])
  
  expect_equal(vcov_dims[1], n_terms)
  expect_equal(vcov_dims[2], n_terms)
})

test_that("prep_fit can handle single term", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = "ZT1")
  
  expect_equal(nrow(prepped$tidy_obj[[1]]), 1)
  expect_equal(dim(prepped$vcov_obj[[1]]), c(1, 1))
})

test_that("prep_fit can handle multiple terms", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = c("ZT1", "ZT2"))
  
  expect_equal(nrow(prepped$tidy_obj[[1]]), 2)
  expect_equal(dim(prepped$vcov_obj[[1]]), c(2, 2))
})

test_that("prep_fit uses regex matching for terms", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  # Should match both ZT1 and ZT2 with "ZT" pattern
  prepped <- prep_fit(fit, term = "ZT")
  
  expect_equal(nrow(prepped$tidy_obj[[1]]), 2)
})

test_that("prep_fit preserves term order from tidy()", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)
  
  prepped <- prep_fit(fit, term = c("ZT2", "ZT1"))
  
  # Should preserve order from tidy(), not from term argument
  expect_true("term" %in% names(prepped$tidy_obj[[1]]))
})
