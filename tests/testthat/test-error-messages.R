# Guidance bullets must reach the user ----
#
# rlang::abort(msg, "i" = hint) puts the hint in `...`, where it becomes a
# condition field and is never printed. The bullets only render when they are
# part of the message vector: rlang::abort(c(msg, "i" = hint)). Every message
# below carries a hint that a caller needs, so each test asserts on the hint
# text rather than the headline.

test_that("ingestion errors keep their `prep_fit()` hint", {
  expect_error(
    as_estimates_vcov(data.frame(a = 1)),
    "Did you pass the result of"
  )
  expect_error(
    get_estimates_df(data.frame(a = 1)),
    "Did you pass the result of"
  )
  expect_error(
    get_glance_df(data.frame(a = 1)),
    "Did you pass the result of"
  )
  expect_error(
    get_vcov(data.frame(a = 1)),
    "Did you pass the result of"
  )
})

test_that("dimension errors report the dimensions that disagree", {
  expect_error(
    make_estimates_vcov(data.frame(estimate = 1:3), diag(2)),
    "estimates_df has 3 rows, vcov_matrix has 2 rows"
  )
  expect_error(
    make_estimates_vcov(data.frame(estimate = 1:2), matrix(1, 2, 3)),
    "vcov_matrix has dimensions 2 x 3"
  )

  prepped <- prep_fit(
    lm(Y ~ Z, data.frame(Y = rnorm(20), Z = rep(c("a", "b"), 10))),
    term = "Zb"
  )
  prepped$vcov_obj <- list(diag(3))
  expect_error(as_estimates_vcov(prepped), "estimates has 1 rows, vcov has 3 rows")
})

test_that("get_glance_df on an estimates_vcov says where to get glance instead", {
  ev <- as_estimates_vcov(prep_fit(
    lm(Y ~ Z, data.frame(Y = rnorm(20), Z = rep(c("a", "b"), 10))),
    term = "Zb"
  ))
  expect_warning(get_glance_df(ev), "Extract glance from prepped_fits")
})

test_that("prep_fit extraction failures name the missing method", {
  broken <- structure(list(), class = "metaprep_no_glance")
  registerS3method(
    "tidy", "metaprep_no_glance",
    function(x, ...) tibble::tibble(term = "a", estimate = 1),
    asNamespace("broom")
  )
  expect_warning(
    try(prep_fit(broken, term = "a"), silent = TRUE),
    "Make sure the model has a glance\\(\\) method available"
  )
})

test_that("missing-package errors say how to install", {
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  ev <- as_estimates_vcov(prep_fit(
    lm(Y ~ Z, data.frame(Y = rnorm(20), Z = rep(c("a", "b"), 10))),
    term = "Zb"
  ))
  expect_error(
    rma_mv_helper(ev, yi = estimate),
    "install.packages\\('metafor'\\)"
  )
  expect_error(
    rma_uni_helper(ev, yi = estimate),
    "install.packages\\('metafor'\\)"
  )
})

test_that("clubSandwich error offers the CR0 fallback", {
  skip_if_not_installed("metafor")
  # rma.mv needs k > 1, so pool two arms rather than one
  ev <- as_estimates_vcov(prep_fit(
    lm(Y ~ Z, data.frame(Y = rnorm(30), Z = rep(c("a", "b", "c"), 10))),
    term = c("Zb", "Zc")
  ))
  fit <- rma_mv_helper(ev, yi = estimate)
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_error(
    metaprep:::rma_robust(fit, cluster = 1, clubSandwich = TRUE),
    "clubSandwich = FALSE for the CR0 estimator"
  )
})
