#' dplyr methods for estimates_vcov objects
#'
#' @description
#' These methods allow standard dplyr operations on estimates_vcov objects while
#' keeping the variance-covariance matrix synchronized with the data.
#'
#' Supported operations:
#' - Row operations that maintain sync: `filter()`, `slice()`, `arrange()`
#' - Column operations that don't change rows: `mutate()`, `rename()`, `select()`
#' - Grouping operation: `nest_by()` (returns rowwise tibble with nested estimates_vcov)
#'
#' @details
#' These methods keep the vcov **row-aligned** (subsetting and reordering the rows
#' also subset/reorder the matrix), but they never **transform** the matrix. In
#' particular `mutate()` can change the *values* of a column without touching the
#' vcov, so `mutate(estimate = -estimate)` (or any rescaling of `estimate`) leaves
#' the vcov inconsistent with the estimates. To sign-flip or rescale estimates and
#' keep the covariances valid, use [rescale_estimates_vcov()], which updates the
#' vcov as \eqn{\mathrm{diag}(s)\, V\, \mathrm{diag}(s)}.
#'
#' @name dplyr-methods
NULL

# ---- Filter ----
#' @export
#' @importFrom dplyr filter
filter.estimates_vcov <- function(.data, ..., .preserve = FALSE) {
  # Apply filter to estimates and track which rows are kept
  # Note: id column is already present in .data$estimates
  filtered_estimates <- .data$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::filter(..., .preserve = .preserve)

  kept_idx <- filtered_estimates$.orig_row

  # Subset vcov matrix (using kept_idx, not id values)
  new_vcov <- .data$vcov[kept_idx, kept_idx, drop = FALSE]

  # Remove tracking column
  filtered_estimates <- dplyr::select(filtered_estimates, -.orig_row)

  new_estimates_vcov(filtered_estimates, new_vcov, row_map = kept_idx)
}

# ---- Slice ----
#' @export
#' @importFrom dplyr slice
slice.estimates_vcov <- function(.data, ..., .preserve = FALSE) {
  # Apply slice to data and track which rows are kept
  sliced_estimates <- .data$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::slice(..., .preserve = .preserve)

  kept_idx <- sliced_estimates$.orig_row

  # Subset vcov matrix
  new_vcov <- .data$vcov[kept_idx, kept_idx, drop = FALSE]

  # Remove tracking column
  sliced_estimates <- dplyr::select(sliced_estimates, -.orig_row)

  new_estimates_vcov(sliced_estimates, new_vcov, row_map = kept_idx)
}

# ---- Slice_head ----
#' @export
#' @importFrom dplyr slice_head
slice_head.estimates_vcov <- function(.data, ..., n, prop) {
  sliced_estimates <- .data$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::slice_head(..., n = n, prop = prop)

  kept_idx <- sliced_estimates$.orig_row
  new_vcov <- .data$vcov[kept_idx, kept_idx, drop = FALSE]
  sliced_estimates <- dplyr::select(sliced_estimates, -.orig_row)

  new_estimates_vcov(sliced_estimates, new_vcov, row_map = kept_idx)
}

# ---- Slice_tail ----
#' @export
#' @importFrom dplyr slice_tail
slice_tail.estimates_vcov <- function(.data, ..., n, prop) {
  sliced_estimates <- .data$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::slice_tail(..., n = n, prop = prop)

  kept_idx <- sliced_estimates$.orig_row
  new_vcov <- .data$vcov[kept_idx, kept_idx, drop = FALSE]
  sliced_estimates <- dplyr::select(sliced_estimates, -.orig_row)

  new_estimates_vcov(sliced_estimates, new_vcov, row_map = kept_idx)
}

# ---- Arrange ----
#' @export
#' @importFrom dplyr arrange
arrange.estimates_vcov <- function(.data, ..., .by_group = FALSE) {
  # Apply arrange to data and track the new order
  arranged_estimates <- .data$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::arrange(..., .by_group = .by_group)

  new_order <- arranged_estimates$.orig_row

  # Reorder vcov matrix rows and columns
  new_vcov <- .data$vcov[new_order, new_order, drop = FALSE]

  # Remove tracking column
  arranged_estimates <- dplyr::select(arranged_estimates, -.orig_row)

  new_estimates_vcov(arranged_estimates, new_vcov, row_map = .data$row_map[new_order])
}

# ---- Mutate ----
#' @export
#' @importFrom dplyr mutate
mutate.estimates_vcov <- function(.data, ...) {
  new_estimates <- dplyr::mutate(.data$estimates, ...)
  new_estimates_vcov(new_estimates, .data$vcov, .data$row_map)
}

# ---- Select ----
#' @export
#' @importFrom dplyr select
select.estimates_vcov <- function(.data, ...) {
  # Select doesn't change row count, so vcov stays the same
  new_estimates <- dplyr::select(.data$estimates, ...)

  new_estimates_vcov(new_estimates, .data$vcov, .data$row_map)
}

# ---- Rename ----
#' @export
#' @importFrom dplyr rename
rename.estimates_vcov <- function(.data, ...) {
  # Rename doesn't change row count, so vcov stays the same
  new_estimates <- dplyr::rename(.data$estimates, ...)

  new_estimates_vcov(new_estimates, .data$vcov, .data$row_map)
}

# ---- Relocate ----
#' @export
#' @importFrom dplyr relocate
relocate.estimates_vcov <- function(.data, ..., .before = NULL, .after = NULL) {
  # Relocate doesn't change row count, so vcov stays the same
  new_estimates <- dplyr::relocate(.data$estimates, ..., .before = {{.before}}, .after = {{.after}})

  new_estimates_vcov(new_estimates, .data$vcov, .data$row_map)
}

# ---- Pull ----
#' @export
#' @importFrom dplyr pull
pull.estimates_vcov <- function(.data, var = -1, name = NULL, ...) {
  # Pull returns a vector, not an estimates_vcov object
  dplyr::pull(.data$estimates, var = {{var}}, name = {{name}}, ...)
}

# ---- Nest_by (THE idiomatic grouping approach) ----
#' @export
#' @importFrom dplyr nest_by
nest_by.estimates_vcov <- function(.data, ..., .key = "data", .keep = FALSE) {
  # Store reference to the vcov matrix before grouping
  parent_vcov <- .data$vcov
  parent_row_map <- .data$row_map

  # Add row indices to track groups
  indexed_estimates <- .data$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number())

  # Group the data
  grouped <- indexed_estimates |>
    dplyr::group_by(..., .drop = TRUE)

  # For each group, create a nested estimates_vcov object
  result <- grouped |>
    dplyr::group_modify(~ {
      idx <- .x$.orig_row

      # Create the key column with estimates_vcov object
      # Use parent_vcov from enclosing scope
      out <- tibble::tibble(
        !!rlang::sym(.key) := list(new_estimates_vcov(
          .x |> dplyr::select(-.orig_row),
          parent_vcov[idx, idx, drop = FALSE],
          row_map = idx
        ))
      )
      out
    })

  # Return as rowwise tibble for easy mutate
  result |> dplyr::rowwise()
}


# ---- Joins ----
#' @export
#' @importFrom dplyr left_join
left_join.estimates_vcov <- function(x, y, by = NULL, copy = FALSE,
                                     suffix = c(".x", ".y"), ...,
                                     keep = NULL, na_matches = c("na", "never"),
                                     multiple = "all", unmatched = "drop",
                                     relationship = NULL) {

  # Store original row order
  original_estimates <- x$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number())

  # Perform the join
  joined_estimates <- dplyr::left_join(
    original_estimates, y,
    by = by, copy = copy, suffix = suffix, ...,
    keep = keep, na_matches = na_matches,
    multiple = multiple, unmatched = unmatched,
    relationship = relationship
  )

  # Check that we didn't lose or duplicate rows
  if (nrow(joined_estimates) != nrow(original_estimates)) {
    rlang::abort(c(
      "left_join() changed the number of rows.",
      "i" = sprintf(
        "Original had %d rows, result has %d rows",
        nrow(original_estimates), nrow(joined_estimates)
      ),
      "i" = "This breaks vcov synchronization. Check your join keys or use multiple = 'first'/'last'."
    ))
  }

  # Remove tracking column -- left_join preserves left table row order
  joined_estimates <- dplyr::select(joined_estimates, -.orig_row)

  new_estimates_vcov(joined_estimates, x$vcov, x$row_map)
}

#' @export
#' @importFrom dplyr right_join
right_join.estimates_vcov <- function(x, y, by = NULL, copy = FALSE,
                                      suffix = c(".x", ".y"), ...,
                                      keep = NULL, na_matches = c("na", "never"),
                                      multiple = "all", unmatched = "drop",
                                      relationship = NULL) {
  rlang::abort(c(
    "right_join() is not supported for estimates_vcov objects.",
    "i" = "Use left_join(y, x) instead if you need right join behavior.",
    "i" = "right_join() would change which rows are kept, breaking vcov sync."
  ))
}

#' @export
#' @importFrom dplyr inner_join
inner_join.estimates_vcov <- function(x, y, by = NULL, copy = FALSE,
                                      suffix = c(".x", ".y"), ...,
                                      keep = NULL, na_matches = c("na", "never"),
                                      multiple = "all", unmatched = "drop",
                                      relationship = NULL) {
  rlang::abort(c(
    "inner_join() is not supported for estimates_vcov objects.",
    "i" = "inner_join() can drop rows, which breaks vcov synchronization.",
    "i" = "Use filter() instead to remove rows explicitly."
  ))
}

#' @export
#' @importFrom dplyr full_join
full_join.estimates_vcov <- function(x, y, by = NULL, copy = FALSE,
                                     suffix = c(".x", ".y"), ...,
                                     keep = NULL, na_matches = c("na", "never"),
                                     multiple = "all", unmatched = "drop",
                                     relationship = NULL) {
  rlang::abort(c(
    "full_join() is not supported for estimates_vcov objects.",
    "i" = "full_join() can add rows, which breaks vcov synchronization.",
    "i" = "Use left_join() to add columns from y to x without adding rows."
  ))
}

#' @export
#' @importFrom dplyr semi_join
semi_join.estimates_vcov <- function(x, y, by = NULL, copy = FALSE, ...,
                                     na_matches = c("na", "never")) {
  # semi_join filters but doesn't add columns, so it's like filter()
  # Store original row indices
  original_estimates <- x$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number())

  # Perform semi join
  filtered_estimates <- dplyr::semi_join(
    original_estimates, y,
    by = by, copy = copy, ...,
    na_matches = na_matches
  )

  # Get kept indices
  kept_idx <- filtered_estimates$.orig_row

  # Subset vcov
  new_vcov <- x$vcov[kept_idx, kept_idx, drop = FALSE]

  # Remove tracking column
  filtered_estimates <- dplyr::select(filtered_estimates, -.orig_row)

  new_estimates_vcov(filtered_estimates, new_vcov, kept_idx)
}

#' @export
#' @importFrom dplyr anti_join
anti_join.estimates_vcov <- function(x, y, by = NULL, copy = FALSE, ...,
                                     na_matches = c("na", "never")) {
  # anti_join filters but doesn't add columns, so it's like filter()
  # Store original row indices
  original_estimates <- x$estimates |>
    dplyr::mutate(.orig_row = dplyr::row_number())

  # Perform anti join
  filtered_estimates <- dplyr::anti_join(
    original_estimates, y,
    by = by, copy = copy, ...,
    na_matches = na_matches
  )

  # Get kept indices
  kept_idx <- filtered_estimates$.orig_row

  # Subset vcov
  new_vcov <- x$vcov[kept_idx, kept_idx, drop = FALSE]

  # Remove tracking column
  filtered_estimates <- dplyr::select(filtered_estimates, -.orig_row)

  new_estimates_vcov(filtered_estimates, new_vcov, kept_idx)
}
