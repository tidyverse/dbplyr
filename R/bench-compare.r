all_equal <- function(x, y) {
  out <- dplyr::all_equal(collect(x), collect(y))

  if (isTRUE(out)) {
    TRUE
  } else {
    structure(FALSE, comment = out)
  }
}

compare_tbls <- function(tbls, op = force, ref = NULL, compare = all_equal, ...) {
  results <- lapply(tbls, op)
  expect_equal_tbls(results, ref = ref, compare = compare, ...)
}

compare_tbls2 <- function(tbls_x, tbls_y, op, ref = NULL, compare = all_equal, ...) {
  results <- Map(op, tbls_x, tbls_y)
  expect_equal_tbls(results, ref = ref, compare = compare, ...)
}

expect_equal_tbls <- function(results, ref = NULL, compare = all_equal, ...) {
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

    msg <- paste0(
      names(rest)[[i]], " not equal to ", ref_name, "\n",
      paste(attr(ok, "comment"), collapse = "\n")
    )
    testthat::expect_true(isTRUE(ok), info = msg)
  }

  invisible(TRUE)
}

eval_tbls <- function(tbls, op) {
  lapply(tbls, function(x) collect(op(x)))
}

eval_tbls2 <- function(tbls_x, tbls_y, op) {
  Map(function(x, y) as.data.frame(op(x, y)), tbls_x, tbls_y)
}
