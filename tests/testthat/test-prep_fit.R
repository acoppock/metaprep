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

test_that("prep_fit warns and continues when glance method fails", {
  warn_fit <- structure(list(), class = "has_tidy_bad_glance")
  tidy.has_tidy_bad_glance <<- function(x, ...) {
    tibble::tibble(term = "x", estimate = 1.0, std.error = 0.1,
                   statistic = 10.0, p.value = 0.001)
  }
  glance.has_tidy_bad_glance <<- function(x, ...) stop("no glance available")
  vcov.has_tidy_bad_glance <<- function(x, ...) {
    m <- matrix(0.01, 1, 1)
    rownames(m) <- colnames(m) <- "x"
    m
  }
  on.exit({
    rm(tidy.has_tidy_bad_glance, glance.has_tidy_bad_glance,
       vcov.has_tidy_bad_glance, envir = .GlobalEnv)
  })

  expect_warning(
    result <- prep_fit(warn_fit, term = "x"),
    "Could not extract glance"
  )
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(result)))
  expect_equal(nrow(result$tidy_obj[[1]]), 1)
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

test_that("prep_fit exact match does not pick up partial matches", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)

  # "ZT" should not match "ZT1" or "ZT2" under exact matching
  prepped <- prep_fit(fit, term = "ZT")
  expect_equal(nrow(prepped$tidy_obj[[1]]), 0)

  # "ZT1" should match only ZT1, not ZT2
  prepped2 <- prep_fit(fit, term = "ZT1")
  expect_equal(nrow(prepped2$tidy_obj[[1]]), 1)
  expect_equal(prepped2$tidy_obj[[1]]$term, "ZT1")
})

test_that("prep_fit regex match picks up prefix patterns", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)

  # "ZT" as regex should match both ZT1 and ZT2
  prepped <- prep_fit(fit, term = "ZT", match = "regex")
  expect_equal(nrow(prepped$tidy_obj[[1]]), 2)
})

test_that("prep_fit validates match argument", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)

  expect_error(prep_fit(fit, term = "ZT1", match = "partial"))
})

test_that("prep_fit preserves term order from tidy()", {
  dat <- make_test_data()
  fit <- lm_robust(Y ~ Z, data = dat)

  prepped <- prep_fit(fit, term = c("ZT2", "ZT1"))

  # Should preserve order from tidy(), not from term argument
  expect_true("term" %in% names(prepped$tidy_obj[[1]]))
})

# ==============================================================================
# Multivariate Model Tests (Method 1: lm_robust with multiple outcomes)
# ==============================================================================

test_that("prep_fit handles multivariate lm_robust (Method 1)", {
  skip_if_not_installed("estimatr")
  set.seed(42)
  dat <- data.frame(
    Y1 = rnorm(100),
    Y2 = rnorm(100),
    Z = factor(sample(c("T0", "T1"), 100, TRUE))
  )
  fit <- estimatr::lm_robust(cbind(Y1, Y2) ~ Z, data = dat)

  expect_true(length(fit$outcome) > 1)

  result <- suppressWarnings(prep_fit(fit, term = "ZT1", match = "regex"))
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(result)))
  expect_equal(nrow(result$tidy_obj[[1]]), 2)
  expect_true(all(grepl("ZT1", result$tidy_obj[[1]]$term)))
  expect_equal(dim(result$vcov_obj[[1]]), c(2, 2))
})

test_that("prep_fit handles multivariate lm_robust with exact term match", {
  skip_if_not_installed("estimatr")
  set.seed(42)
  dat <- data.frame(
    Y1 = rnorm(100),
    Y2 = rnorm(100),
    Z = factor(sample(c("T0", "T1"), 100, TRUE))
  )
  fit <- estimatr::lm_robust(cbind(Y1, Y2) ~ Z, data = dat)

  result <- suppressWarnings(prep_fit(fit, term = "Y1:ZT1", match = "exact"))
  expect_equal(nrow(result$tidy_obj[[1]]), 1)
  expect_equal(result$tidy_obj[[1]]$term, "Y1:ZT1")
  expect_equal(dim(result$vcov_obj[[1]]), c(1, 1))
})

# ==============================================================================
# Multivariate Model Tests (Method 2: tidy has "outcome" column)
# ==============================================================================

test_that("prep_fit handles models with outcome column in tidy (Method 2)", {
  multi_out_fit <- structure(list(), class = "multi_outcome_tidy_model")
  tidy.multi_outcome_tidy_model <<- function(x, ...) {
    tibble::tibble(
      outcome = rep(c("A", "B"), each = 2),
      term = rep(c("(Intercept)", "treat"), 2),
      estimate = c(0.1, 0.3, 0.2, 0.4),
      std.error = rep(0.05, 4),
      statistic = c(2, 6, 4, 8),
      p.value = rep(0.05, 4)
    )
  }
  vcov.multi_outcome_tidy_model <<- function(x, ...) {
    terms <- paste0(rep(c("A", "B"), each = 2), ":", rep(c("(Intercept)", "treat"), 2))
    m <- diag(4)
    rownames(m) <- colnames(m) <- terms
    m
  }
  glance.multi_outcome_tidy_model <<- function(x, ...) {
    data.frame(nobs = 100L)
  }
  on.exit({
    rm(tidy.multi_outcome_tidy_model, vcov.multi_outcome_tidy_model,
       glance.multi_outcome_tidy_model, envir = .GlobalEnv)
  })

  result <- prep_fit(multi_out_fit, term = "A:treat", match = "exact")
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(result)))
  expect_equal(nrow(result$tidy_obj[[1]]), 1)
  expect_equal(result$tidy_obj[[1]]$term, "A:treat")
  expect_equal(dim(result$vcov_obj[[1]]), c(1, 1))
})

# ==============================================================================
# Multivariate Model Tests (Method 3: tidy has "response" column)
# ==============================================================================

test_that("prep_fit handles models with response column in tidy (Method 3)", {
  multi_resp_fit <- structure(list(), class = "multi_response_tidy_model")
  tidy.multi_response_tidy_model <<- function(x, ...) {
    tibble::tibble(
      response = rep(c("A", "B"), each = 2),
      term = rep(c("(Intercept)", "treat"), 2),
      estimate = c(0.1, 0.3, 0.2, 0.4),
      std.error = rep(0.05, 4),
      statistic = c(2, 6, 4, 8),
      p.value = rep(0.05, 4)
    )
  }
  vcov.multi_response_tidy_model <<- function(x, ...) {
    terms <- paste0(rep(c("A", "B"), each = 2), ":", rep(c("(Intercept)", "treat"), 2))
    m <- diag(4)
    rownames(m) <- colnames(m) <- terms
    m
  }
  glance.multi_response_tidy_model <<- function(x, ...) {
    data.frame(nobs = 100L)
  }
  on.exit({
    rm(tidy.multi_response_tidy_model, vcov.multi_response_tidy_model,
       glance.multi_response_tidy_model, envir = .GlobalEnv)
  })

  result <- prep_fit(multi_resp_fit, term = "A:treat", match = "exact")
  expect_true(all(c("tidy_obj", "glance_obj", "vcov_obj") %in% names(result)))
  expect_equal(nrow(result$tidy_obj[[1]]), 1)
  expect_equal(result$tidy_obj[[1]]$term, "A:treat")
  expect_equal(dim(result$vcov_obj[[1]]), c(1, 1))
})
