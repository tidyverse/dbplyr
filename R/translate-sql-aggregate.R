#' SQL helpers for aggregate functions
#'
#' @description
#' These functions help you create custom aggregate SQL translations when
#' implementing a new backend. They are typically used within [sql_translator()]
#' to define how R aggregate functions should be translated to SQL.
#'
#' * `sql_aggregate()` creates a SQL aggregate function translator for functions
#'   with a single argument and an optional `na.rm` parameter (e.g., `SUM()`,
#'   `AVG()`).
#' * `sql_aggregate_2()` creates a SQL aggregate function translator for
#'   functions with exactly two arguments (e.g., `CORR()`, `COVAR_SAMP()`).
#' * `sql_aggregate_n()` creates a SQL aggregate function translator for
#'   functions with any number of arguments and an optional `na.rm` parameter
#'   (e.g., `LEAST()`, `GREATEST()`).
#' * `sql_not_supported()` creates a function that throws an informative error
#'   when a function is not supported in SQL.
#' * `sql_check_na_rm()` is a helper that you can use in aggregate functions
#'   to direct the user towards setting `na.rm = TRUE`.
#'
#' @param f The name of the SQL function as a string.
#' @param f_r The name of the R function being translated as a string.
#' @family SQL translation helpers
#' @name sql_translation_agg
NULL

#' @rdname sql_translation_agg
#' @export
sql_aggregate <- function(f, f_r = f) {
  check_string(f)

  function(x, na.rm = FALSE) {
    sql_check_na_rm(na.rm)
    sql_glue("{.sql f}({x})")
  }
}

#' @rdname sql_translation_agg
#' @export
sql_aggregate_2 <- function(f) {
  check_string(f)

  function(x, y) {
    sql_glue("{.sql f}({x}, {y})")
  }
}

#' @rdname sql_translation_agg
#' @export
sql_aggregate_n <- function(f, f_r = f) {
  check_string(f)

  function(..., na.rm = FALSE) {
    sql_check_na_rm(na.rm)
    dots <- list(...)
    sql_glue("{.sql f}({dots})")
  }
}

sql_aggregate_win <- function(f) {
  force(f)

  function(...) {
    cli_abort(
      "{.fun {f}} is only available in a windowed ({.fun mutate}) context"
    )
  }
}

#' @rdname sql_translation_agg
#' @param na.rm Logical indicating whether missing values should be removed.
#'   In SQL, missing values are always removed in aggregate functions, so this
#'   function will warn if `na.rm` is not `TRUE`.
#' @export
sql_check_na_rm <- function(na.rm) {
  if (identical(na.rm, TRUE) || is_testing()) {
    return()
  }

  cli::cli_warn(
    c(
      "Missing values are always removed in SQL aggregation functions.",
      "Use {.code na.rm = TRUE} to silence this warning"
    ),
    .frequency = "regularly",
    .frequency_id = "dbplyr_check_na_rm"
  )
}

#' @rdname sql_translation_agg
#' @export
sql_not_supported <- function(f) {
  check_string(f)

  function(...) {
    cli_abort(
      "{.fun {f}} is not available in this SQL variant.",
      class = "dbplyr_error_unsupported_fn"
    )
  }
}

sql_agg_not_supported <- function(f, backend) {
  check_string(f)

  msg <- "Translation of {.fun {f}} in {.fun summarise} is not supported"

  if (is.null(backend)) {
    msg <- paste0(msg, " on database backends.")
  } else {
    msg <- paste0(msg, " for {backend}.")
  }

  function(...) {
    dots <- enexprs(...)
    f_call_str <- as_label(call2(f, !!!dots))
    msg <- c(
      msg,
      i = "Use a combination of {.fun distinct} and {.fun mutate} for the same result:",
      " " = "{.code mutate(<col> = {f_call_str}) |> distinct(<col>)}"
    )
    cli_abort(msg)
  }
}

sql_win_not_supported <- function(f, backend) {
  check_string(f)

  msg <- "Translation of {.fun {f}} in {.fun mutate} is not supported"

  if (is.null(backend)) {
    msg <- paste0(msg, " on database backends.")
  } else {
    msg <- paste0(msg, " for {backend}.")
  }

  function(...) {
    dots <- enexprs(...)
    f_call_str <- as_label(call2(f, !!!dots))
    msg <- c(
      msg,
      i = "Use a combination of {.fun summarise} and {.fun left_join} instead:",
      " " = "{.code df |> left_join(summarise(<col> = {f_call_str}))}."
    )
    cli_abort(msg)
  }
}

sql_quantile <- function(f, style = c("infix", "ordered"), window = FALSE) {
  force(f)
  style <- match.arg(style)
  force(window)

  function(x, probs, na.rm = FALSE) {
    check_probs(probs)
    sql_check_na_rm(na.rm)

    if (style == "infix") {
      sql <- sql_call2(f, x, probs)
    } else {
      call <- sql_call2(f, probs)
      sql <- sql_glue("{call} WITHIN GROUP (ORDER BY {x})")
    }

    if (window) {
      sql <- win_over(
        sql,
        partition = win_current_group(),
        frame = win_current_frame()
      )
    }
    sql
  }
}

sql_median <- function(f, style = c("infix", "ordered"), window = FALSE) {
  quantile <- sql_quantile(f, style = style, window = window)
  function(x, na.rm = FALSE) {
    quantile(x, 0.5, na.rm = na.rm)
  }
}

check_probs <- function(probs, call = caller_env()) {
  # TODO min, max? Inf? NA?
  check_number_decimal(probs, call = call)

  if (length(probs) > 1) {
    cli_abort("SQL translation only supports single value for {.arg probs}.")
  }
}
