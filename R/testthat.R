
expect_equal_tbl <- function(object, expected, ...,
                             info = NULL, label = NULL, expected.label = NULL) {
  lab_act <- label %||% expr_label(substitute(object))
  lab_exp <- expected.label %||% expr_label(substitute(expected))

  ok <- dplyr::all_equal(collect(object), collect(expected), ...)
  msg <- glue("
    {lab_act} not equal to {lab_exp}.
    {paste(ok, collapse = '\n')}
  ")
  testthat::expect(isTRUE(ok), msg, info = info)
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
    expect_equal_tbl(
      rest[[i]], ref, ...,
      label = names(rest)[[i]],
      expected.label = ref_name
    )
  }

  invisible(TRUE)
}
