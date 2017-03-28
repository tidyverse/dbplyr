compare_tbls <- function(tbls, op, ref = NULL, compare = equal_data_frame, ...) {
  results <- eval_tbls(tbls, op)
  expect_equal_tbls(results, compare = compare, ...)
}

compare_tbls2 <- function(tbls_x, tbls_y, op, ref = NULL, compare = equal_data_frame, ...) {
  results <- eval_tbls2(tbls_x, tbls_y, op)
  expect_equal_tbls(results, compare = compare, ...)
}

expect_equal_tbls <- function(results, ref = NULL, compare = equal_data_frame, ...) {
  if (length(results) < 2 && is.null(ref)) {
    testthat::skip("Need at least two srcs to compare")
  }


  if (is.null(ref)) {
    ref <- results[[1]]
    ref_name <- names(results)[1]
    rest <- results[-1]
  } else {
    rest <- results
    ref_name <- "supplied comparison"
  }

  for (i in seq_along(rest)) {
    ok <- compare(ref, rest[[i]], ...)
    # if (!ok) browser()
    msg <- paste0(
      names(rest)[[i]], " not equal to ", ref_name, "\n",
      attr(ok, "comment")
    )
    testthat::expect_true(ok, info = msg)
  }

  invisible(TRUE)
}

eval_tbls <- function(tbls, op) {
  lapply(tbls, function(x) collect(op(x)))
}

eval_tbls2 <- function(tbls_x, tbls_y, op) {
  Map(function(x, y) dplyr(op(x, y)), tbls_x, tbls_y)
}
