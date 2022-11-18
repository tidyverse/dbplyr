
compare_tbl <- function(x, y, label = NULL, expected.label = NULL) {
  testthat::expect_equal(
    arrange(collect(x), dplyr::across(everything())),
    arrange(collect(y), dplyr::across(everything())),
    label = label,
    expected.label = expected.label
  )
}

expect_equal_tbls <- function(results, ref = NULL, ...) {
  stopifnot(is.list(results))

  if (!is_named(results)) {
    result_name <- expr_name(substitute(results)) # nocov
    names(results) <- paste0(result_name, "_", seq_along(results)) # nocov
  }

  # If ref is NULL, use the first result
  if (is.null(ref)) {
    if (length(results) < 2) {
      testthat::skip("Need at least two srcs to compare")
    }

    ref <- results[[1]]
    ref_name <- names(results)[[1]]

    rest <- results[-1]
  } else {
    rest <- results
    ref_name <- "`ref`"
  }

  for (i in seq_along(rest)) {
    compare_tbl(
      rest[[i]], ref,
      label = names(rest)[[i]],
      expected.label = ref_name
    )
  }

  invisible(TRUE)
}
