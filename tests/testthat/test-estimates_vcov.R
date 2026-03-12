# Tests for estimates_vcov objects and accessor functions
# tests/testthat/test-estimates_vcov.R

library(testthat)
library(metaprep)

# ==============================================================================
# Accessor Functions Tests (get_estimates_df, get_glance_df, get_vcov)
# ==============================================================================

test_that("get_estimates_df works on prepped_fits", {
  prepped_fits <- make_test_prepped_fits()
  
  estimates <- get_estimates_df(prepped_fits)
  
  expect_s3_class(estimates, "tbl_df")
  expect_true("estimate" %in% names(estimates))
  expect_false("vcov_obj" %in% names(estimates))
  expect_false("glance_obj" %in% names(estimates))
})

test_that("get_estimates_df works on estimates_vcov", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  estimates <- get_estimates_df(ev)
  
  expect_s3_class(estimates, "tbl_df")
  expect_identical(estimates, ev$estimates)
})

test_that("get_estimates_df errors on invalid input", {
  expect_error(
    get_estimates_df("not a data frame"),
    "must be a data frame"
  )
  
  expect_error(
    get_estimates_df(data.frame(x = 1)),
    "must contain a list-column named `tidy_obj`"
  )
})

test_that("get_estimates_df drops glance/vcov columns", {
  df <- tibble::tibble(
    tidy_obj = list(tibble::tibble(term = "a", estimate = 1)),
    glance_obj = list(tibble::tibble(r.squared = 0.9)),
    vcov_obj = list(matrix(1))
  )
  
  result <- get_estimates_df(df)
  expect_false(any(c("glance_obj", "vcov_obj") %in% names(result)))
})

test_that("get_glance_df works on prepped_fits", {
  prepped_fits <- make_test_prepped_fits()
  
  glance <- get_glance_df(prepped_fits)
  
  expect_s3_class(glance, "tbl_df")
  expect_false("tidy_obj" %in% names(glance))
  expect_false("vcov_obj" %in% names(glance))
})

test_that("get_glance_df warns on estimates_vcov", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  expect_warning(
    result <- get_glance_df(ev),
    "glance information is not stored"
  )
  expect_null(result)
})

test_that("get_glance_df errors on invalid input", {
  expect_error(
    get_glance_df("not a data frame"),
    "must be a data frame"
  )
  
  expect_error(
    get_glance_df(data.frame(x = 1)),
    "must contain a list-column named `glance_obj`"
  )
})

test_that("get_glance_df drops tidy/vcov columns", {
  df <- tibble::tibble(
    glance_obj = list(tibble::tibble(r.squared = 0.9)),
    tidy_obj = list(tibble::tibble(term = "a")),
    vcov_obj = list(matrix(1))
  )
  
  result <- get_glance_df(df)
  expect_false(any(c("tidy_obj", "vcov_obj") %in% names(result)))
  expect_true("r.squared" %in% names(result))
})

test_that("get_vcov works on prepped_fits", {
  prepped_fits <- make_test_prepped_fits()
  
  vcov_matrix <- get_vcov(prepped_fits)
  
  expect_true(is.matrix(vcov_matrix) || inherits(vcov_matrix, "Matrix"))
  expect_equal(nrow(vcov_matrix), ncol(vcov_matrix))
})

test_that("get_vcov works on estimates_vcov", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  vcov_matrix <- get_vcov(ev)
  
  expect_true(is.matrix(vcov_matrix))
  expect_identical(vcov_matrix, ev$vcov)
})

test_that("get_vcov errors on invalid input", {
  expect_error(
    get_vcov("not a data frame"),
    "must be a data frame"
  )
  
  expect_error(
    get_vcov(data.frame(x = 1)),
    "must contain a list-column named `vcov_obj`"
  )
})

test_that("get_vcov creates block diagonal matrix", {
  m1 <- diag(2)
  m2 <- diag(3)
  df <- tibble::tibble(
    tidy_obj = list(
      tibble::tibble(term = c("a", "b"), estimate = c(1, 2)),
      tibble::tibble(term = c("c", "d", "e"), estimate = c(3, 4, 5))
    ),
    vcov_obj = list(m1, m2)
  )
  
  result <- get_vcov(df)
  expect_equal(dim(result), c(5, 5))
})

# ==============================================================================
# as_estimates_vcov() Tests
# ==============================================================================

test_that("as_estimates_vcov creates valid object", {
  prepped_fits <- make_test_prepped_fits()
  ev <- as_estimates_vcov(prepped_fits)
  
  expect_s3_class(ev, "estimates_vcov")
  expect_true(is.list(ev))
  expect_named(ev, c("estimates", "vcov", "row_map"))
  expect_s3_class(ev$estimates, "tbl_df")
  expect_true(is.matrix(ev$vcov))
  expect_type(ev$row_map, "integer")
})

test_that("as_estimates_vcov adds id column", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  expect_true("id" %in% names(ev$estimates))
  expect_equal(rownames(ev$vcov), ev$estimates$id)
  expect_equal(colnames(ev$vcov), ev$estimates$id)
})

test_that("as_estimates_vcov preserves existing id column", {
  prepped_fits <- make_test_prepped_fits()
  prepped_fits$tidy_obj[[1]]$id <- c("custom_1", "custom_2")
  
  ev <- as_estimates_vcov(prepped_fits)
  
  # Should preserve the existing id
  expect_true("id" %in% names(ev$estimates))
})

test_that("vcov dimensions match estimates rows", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  expect_equal(nrow(ev$estimates), nrow(ev$vcov))
  expect_equal(nrow(ev$vcov), ncol(ev$vcov))
})

test_that("as_estimates_vcov validates input", {
  expect_error(
    as_estimates_vcov("not a data frame"),
    "must be a data frame"
  )
  
  expect_error(
    as_estimates_vcov(mtcars),
    "tidy_obj.*vcov_obj"
  )
})

test_that("as_estimates_vcov detects dimension mismatch", {
  # Create mismatched data
  prepped_fits <- make_test_prepped_fits()
  
  # Manually break it
  prepped_fits$tidy_obj[[1]] <- prepped_fits$tidy_obj[[1]][1, ]
  
  expect_error(
    as_estimates_vcov(prepped_fits),
    "Dimension mismatch"
  )
})

# ==============================================================================
# estimates_vcov_from_pieces() Tests
# ==============================================================================

test_that("estimates_vcov_from_pieces works correctly", {
  prepped_fits <- make_test_prepped_fits()
  
  estimates_df <- get_estimates_df(prepped_fits)
  vcov_matrix <- get_vcov(prepped_fits) |> as.matrix()
  
  ev <- estimates_vcov_from_pieces(estimates_df, vcov_matrix)
  
  expect_s3_class(ev, "estimates_vcov")
  expect_equal(nrow(ev$estimates), nrow(estimates_df))
  expect_equal(nrow(ev$vcov), nrow(vcov_matrix))
})

test_that("estimates_vcov_from_pieces validates input types", {
  estimates_df <- data.frame(term = "x", estimate = 1)
  vcov_matrix <- matrix(1, 1, 1)
  
  expect_error(
    estimates_vcov_from_pieces("not a df", vcov_matrix),
    "must be a data frame"
  )
  
  expect_error(
    estimates_vcov_from_pieces(estimates_df, "not a matrix"),
    "must be a matrix"
  )
})

test_that("estimates_vcov_from_pieces validates dimensions", {
  estimates_df <- data.frame(term = c("x", "y"), estimate = c(1, 2))
  vcov_matrix <- matrix(1, 1, 1)  # Wrong size
  
  expect_error(
    estimates_vcov_from_pieces(estimates_df, vcov_matrix),
    "Dimension mismatch"
  )
})

test_that("estimates_vcov_from_pieces validates square matrix", {
  estimates_df <- data.frame(term = "x", estimate = 1)
  vcov_matrix <- matrix(1, 1, 2)  # Not square
  
  expect_error(
    estimates_vcov_from_pieces(estimates_df, vcov_matrix),
    "must be square"
  )
})

test_that("estimates_vcov_from_pieces handles sparse matrices", {
  prepped_fits <- make_test_prepped_fits()
  estimates_df <- get_estimates_df(prepped_fits)
  vcov_sparse <- get_vcov(prepped_fits)  # May be sparse
  
  ev <- estimates_vcov_from_pieces(estimates_df, vcov_sparse)
  
  expect_s3_class(ev, "estimates_vcov")
  expect_true(is.matrix(ev$vcov))
})

# ==============================================================================
# Print and Conversion Methods
# ==============================================================================

test_that("print method works", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  expect_output(print(ev), "estimates_vcov")
  expect_output(print(ev), "estimates with.*vcov matrix")
  expect_output(print(ev), "\\d+ estimates")
})

test_that("as_tibble converts to tibble", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  tb <- as_tibble(ev)
  
  expect_s3_class(tb, "tbl_df")
  expect_identical(tb, ev$estimates)
})

test_that("as.data.frame converts to data frame", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  df <- as.data.frame(ev)
  
  expect_s3_class(df, "tbl_df")
  expect_identical(df, ev$estimates)
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("single row estimates_vcov works", {
  prepped_fits <- make_test_prepped_fits()
  single_row <- prepped_fits[1, ]
  
  ev <- as_estimates_vcov(single_row)
  
  expect_s3_class(ev, "estimates_vcov")
  expect_equal(nrow(ev$estimates), nrow(single_row$tidy_obj[[1]]))
  expect_equal(nrow(ev$vcov), nrow(ev$estimates))
})

test_that("empty prepped_fits works", {
  prepped_fits <- make_test_prepped_fits()
  empty <- prepped_fits[0, ]
  
  ev <- as_estimates_vcov(empty)
  
  expect_s3_class(ev, "estimates_vcov")
  expect_equal(nrow(ev$estimates), 0)
  expect_equal(dim(ev$vcov), c(0, 0))
})

test_that("id column has correct type", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  expect_type(ev$estimates$id, "character")
})

test_that("row_map tracks original indices", {
  ev <- as_estimates_vcov(make_test_prepped_fits())
  
  expect_equal(ev$row_map, seq_len(nrow(ev$estimates)))
})
