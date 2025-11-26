sql_cut <- function(
  x,
  breaks,
  labels = NULL,
  include.lowest = FALSE,
  right = TRUE
) {
  breaks <- sort.int(as.double(breaks), na.last = TRUE)
  if (anyDuplicated(breaks)) {
    cli_abort("{.arg breaks} are not unique.")
  }
  n <- length(breaks)
  if (n == 1) {
    cli_abort("{.arg breaks} must have at least two values.")
  }
  if (any(is.na(breaks))) {
    cli_abort("{.arg breaks} values must not be missing.")
  }

  include.lowest <- vctrs::vec_cast(include.lowest, logical())
  vctrs::vec_assert(include.lowest, size = 1)
  labels <- check_cut_labels(labels, breaks, include.lowest, right)

  cases <- purrr::map(
    seq2(1, n - 2L),
    ~ {
      if (right) {
        expr(!!x <= !!breaks[[.x + 1]] ~ !!labels[[.x]])
      } else {
        expr(!!x < !!breaks[[.x + 1]] ~ !!labels[[.x]])
      }
    }
  )

  if (is.finite(breaks[[1]])) {
    if (right && !include.lowest) {
      first_case <- expr(!!x <= !!breaks[[1]] ~ NA)
    } else {
      first_case <- expr(!!x < !!breaks[[1]] ~ NA)
    }

    cases <- c(first_case, cases)
  }

  if (is.finite(breaks[[n]])) {
    if (right || include.lowest) {
      last_cases <- exprs(
        !!x <= !!breaks[[n]] ~ !!labels[[n - 1]],
        !!x > !!breaks[[n]] ~ NA
      )
    } else {
      last_cases <- exprs(
        !!x < !!breaks[[n]] ~ !!labels[[n - 1]],
        !!x >= !!breaks[[n]] ~ NA
      )
    }
  } else {
    if (right) {
      last_cases <- exprs(!!x > !!breaks[[n - 1]] ~ !!labels[[n - 1]])
    } else {
      last_cases <- exprs(!!x >= !!breaks[[n - 1]] ~ !!labels[[n - 1]])
    }
  }

  translate_sql(case_when(!!!cases, !!!last_cases), con = sql_current_con())
}

check_cut_labels <- function(
  labels,
  breaks,
  include.lowest,
  right,
  call = caller_env()
) {
  labels <- labels %||% TRUE

  n <- length(breaks)
  if (vctrs::vec_is(labels, character())) {
    return(vctrs::vec_recycle(labels, n - 1, x_arg = "labels", call = call))
  }

  vctrs::vec_assert(labels, size = 1L)
  if (!vctrs::vec_is(labels, logical()) || is.na(labels)) {
    cli_abort("{.arg labels} must be a bool or a character.", call = call)
  }

  if (is_false(labels)) {
    return(as.character(seq2(1, n)))
  }

  if (right) {
    labels <- paste0("(", breaks[seq2(1, n - 1)], ",", breaks[seq2(2, n)], "]")
  } else {
    labels <- paste0("[", breaks[seq2(1, n - 1)], ",", breaks[seq2(2, n)], ")")
  }

  if (include.lowest) {
    if (right) {
      substr(labels[[1]], 1, 1) <- "["
    } else {
      nchar <- nchar(labels[[n - 1]], "chars")
      substr(labels[[n - 1]], nchar, nchar) <- "]"
    }
  }

  labels
}

utils::globalVariables(c("case_when"))
