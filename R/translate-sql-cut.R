sql_cut <- function(x, breaks, labels = NULL, right = TRUE) {
  breaks <- sort.int(as.double(breaks))
  if (anyDuplicated(breaks)) {
    abort("`breaks` are not unique.")
  }
  n <- length(breaks)
  if (n == 1) {
    abort("`breaks` must have at least two values.")
  }

  labels <- check_cut_labels(labels, breaks, right)

  if (right) {
    op_smaller <- sym("<=")
    op_bigger <- sym(">")
  } else {
    op_smaller <- sym("<")
    op_bigger <- sym(">=")
  }

  if (is.finite(breaks[[1]])) {
    first_case <- expr(!!call2(op_smaller, x, breaks[[1]]) ~ NA)
    start <- 1L
  } else {
    first_case <- expr(!!call2(op_smaller, x, breaks[[2]]) ~ !!labels[[1]])
    start <- 2L
  }

  if (is.finite(breaks[[n]])) {
    last_case <- expr(!!call2(op_bigger, x, breaks[[n]]) ~ NA)
    end <- n - 1L
  } else {
    last_case <- expr(!!call2(op_bigger, x, breaks[[n - 1]]) ~ !!labels[[n - 1]])
    end <- n - 2L
  }

  cases <- purrr::map(
    seq2(start, end),
    ~ {
      cond_lower <- expr(!!call2(op_bigger, x, breaks[[.x]]))
      cond_upper <- expr(!!call2(op_smaller, x, breaks[[.x + 1]]))
      expr(!!cond_lower & !!cond_upper ~ !!labels[[.x]])
    }
  )

  translate_sql(case_when(!!first_case, !!!cases, !!last_case))
}

check_cut_labels <- function(labels, breaks, right) {
  labels <- labels %||% TRUE

  n <- length(breaks)
  if (vctrs::vec_is(labels, character())) {
    return(vctrs::vec_recycle(labels, n - 1, x_arg = "labels"))
  }

  vctrs::vec_assert(labels, size = 1L)
  if (!vctrs::vec_is(labels, logical()) || is.na(labels)) {
    abort("`labels` must be a bool or a character.")
  }

  if (is_false(labels)) {
    return(as.character(seq2(1, n)))
  }

  if (right) {
    paste0("(", breaks[seq2(1, n-1)], ",", breaks[seq2(2, n)], "]")
  } else {
    paste0("[", breaks[seq2(1, n-1)], ",", breaks[seq2(2, n)], ")")
  }
}

globalVariables(c("case_when"))
