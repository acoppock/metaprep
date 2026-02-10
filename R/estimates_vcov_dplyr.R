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
#' @name dplyr-methods
NULL

# ---- Filter ----
#' @export
#' @importFrom dplyr filter
filter.estimates_vcov <- function(.data, ..., .preserve = FALSE) {
  # Apply filter to data and track which rows are kept
  filtered_data <- .data$data |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::filter(..., .preserve = .preserve)

  kept_idx <- filtered_data$.orig_row

  # Subset vcov matrix
  new_vcov <- .data$vcov[kept_idx, kept_idx, drop = FALSE]

  # Remove tracking column
  filtered_data <- dplyr::select(filtered_data, -.orig_row)

  new_estimates_vcov(filtered_data, new_vcov, row_map = kept_idx)
}

# ---- Slice ----
#' @export
#' @importFrom dplyr slice
slice.estimates_vcov <- function(.data, ..., .preserve = FALSE) {
  # Apply slice to data and track which rows are kept
  sliced_data <- .data$data |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::slice(..., .preserve = .preserve)

  kept_idx <- sliced_data$.orig_row

  # Subset vcov matrix
  new_vcov <- .data$vcov[kept_idx, kept_idx, drop = FALSE]

  # Remove tracking column
  sliced_data <- dplyr::select(sliced_data, -.orig_row)

  new_estimates_vcov(sliced_data, new_vcov, row_map = kept_idx)
}

# ---- Arrange ----
#' @export
#' @importFrom dplyr arrange
arrange.estimates_vcov <- function(.data, ..., .by_group = FALSE) {
  # Apply arrange to data and track the new order
  arranged_data <- .data$data |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::arrange(..., .by_group = .by_group)

  new_order <- arranged_data$.orig_row

  # Reorder vcov matrix rows and columns
  new_vcov <- .data$vcov[new_order, new_order, drop = FALSE]

  # Remove tracking column
  arranged_data <- dplyr::select(arranged_data, -.orig_row)

  new_estimates_vcov(arranged_data, new_vcov, row_map = .data$row_map[new_order])
}

# ---- Mutate ----
#' @export
#' @importFrom dplyr mutate
mutate.estimates_vcov <- function(.data, ...) {
  # Mutate doesn't change row count, so vcov stays the same
  new_data <- dplyr::mutate(.data$data, ...)

  # Check that row count didn't change (shouldn't happen with mutate, but be safe)
  if (nrow(new_data) != nrow(.data$data)) {
    rlang::abort("mutate() changed the number of rows. This shouldn't happen!")
  }

  new_estimates_vcov(new_data, .data$vcov, .data$row_map)
}

# ---- Select ----
#' @export
#' @importFrom dplyr select
select.estimates_vcov <- function(.data, ...) {
  # Select doesn't change row count, so vcov stays the same
  new_data <- dplyr::select(.data$data, ...)

  new_estimates_vcov(new_data, .data$vcov, .data$row_map)
}

# ---- Rename ----
#' @export
#' @importFrom dplyr rename
rename.estimates_vcov <- function(.data, ...) {
  # Rename doesn't change row count, so vcov stays the same
  new_data <- dplyr::rename(.data$data, ...)

  new_estimates_vcov(new_data, .data$vcov, .data$row_map)
}

# ---- Relocate ----
#' @export
#' @importFrom dplyr relocate
relocate.estimates_vcov <- function(.data, ..., .before = NULL, .after = NULL) {
  # Relocate doesn't change row count, so vcov stays the same
  new_data <- dplyr::relocate(.data$data, ..., .before = {{.before}}, .after = {{.after}})

  new_estimates_vcov(new_data, .data$vcov, .data$row_map)
}

# ---- Pull ----
#' @export
#' @importFrom dplyr pull
pull.estimates_vcov <- function(.data, var = -1, name = NULL, ...) {
  # Pull returns a vector, not an estimates_vcov object
  dplyr::pull(.data$data, var = {{var}}, name = {{name}}, ...)
}

# ---- Nest_by (THE idiomatic grouping approach) ----
#' @export
#' @importFrom dplyr nest_by
nest_by.estimates_vcov <- function(.data, ..., .key = "data", .keep = FALSE) {
  # Store reference to the vcov matrix before grouping
  parent_vcov <- .data$vcov
  parent_row_map <- .data$row_map
  
  # Add row indices to track groups
  indexed_data <- .data$data |>
    dplyr::mutate(.orig_row = dplyr::row_number())

  # Group the data
  grouped <- indexed_data |>
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

# ---- Ungroup ----
#' @export
#' @importFrom dplyr ungroup
ungroup.estimates_vcov <- function(x, ...) {
  # If already ungrouped, return as-is
  x
}
