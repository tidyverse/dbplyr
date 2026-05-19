sql_case_match <- function(.x, ..., .default = NULL, .ptype = NULL) {
  lifecycle::deprecate_warn("2.6.0", "case_match()", "recode_values()")

  error_call <- current_call()
  check_unsupported_arg(.ptype, call = error_call)
  check_recode_x(enexpr(.x), .x, arg = ".x", error_call = error_call)

  formulas <- list2(...)
  formulas <- purrr::compact(formulas)
  if (length(formulas) == 0) {
    cli_abort("No cases provided", call = error_call)
  }

  con <- sql_current_con()
  clauses <- sql_recode_clauses_formula(
    formulas,
    .x,
    con,
    error_call = error_call
  )
  if (!is_null(.default)) {
    clauses <- c(clauses, paste0("ELSE ", escape(.default, con = con)))
  }

  sql_case_finalize(clauses)
}

sql_case_match_clause <- function(f, x, con) {
  env <- environment(f)
  f_query <- f[[2]]
  if (is_call(f_query, "c")) {
    # need to handle `c(...)` specially because on `expr(c(1, y))` it returns
    # `sql('1', '`y`')`
    f_query <- call_args(f_query)
  } else {
    if (!is_vector(f_query) || length(f_query) == 1) {
      f_query <- list(f_query)
    }
  }

  f_query <- purrr::map_if(
    f_query,
    \(clause) !is_vector(clause),
    \(clause) translate_sql(!!clause, con = con)
  )
  missing_loc <- purrr::map_lgl(f_query, function(clause) {
    is.null(clause) || is.na(clause)
  })

  f_query <- vctrs::vec_slice(f_query, !missing_loc)
  has_na <- any(missing_loc)

  query <- NULL
  if (!is_empty(f_query)) {
    query <- sql_glue("{x} IN {f_query*}")
  }

  if (has_na) {
    query <- paste(c(query, sql_glue2(con, "{x} IS NULL")), collapse = " OR ")
  }

  query
}

sql_recode_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL,
  default = NULL,
  unmatched = "default",
  ptype = NULL
) {
  error_call <- current_call()
  check_unsupported_arg(ptype, call = error_call)
  check_unsupported_arg(unmatched, allowed = "default", call = error_call)
  check_recode_x(enexpr(x), x, error_call = error_call)

  con <- sql_current_con()
  clauses <- sql_recode_clauses(
    ...,
    x = x,
    from = from,
    to = to,
    con = con,
    allow_empty = FALSE,
    error_call = error_call
  )

  if (!is_null(default)) {
    clauses <- c(clauses, paste0("ELSE ", escape(default, con = con)))
  }

  sql_case_finalize(clauses)
}

sql_replace_values <- function(x, ..., from = NULL, to = NULL) {
  error_call <- current_call()
  check_recode_x(enexpr(x), x, error_call = error_call)

  con <- sql_current_con()
  clauses <- sql_recode_clauses(
    ...,
    x = x,
    from = from,
    to = to,
    con = con,
    allow_empty = TRUE,
    error_call = error_call
  )

  if (length(clauses) == 0) {
    return(x)
  }

  clauses <- c(clauses, paste0("ELSE ", escape(x, con = con)))
  sql_case_finalize(clauses)
}

check_recode_x <- function(x_expr, x, arg = "x", error_call = caller_env()) {
  if (!is_symbol(x_expr) && !is_call(x_expr)) {
    cli_abort(
      "{.arg {arg}} must be a variable or function call, not {.obj_type_friendly {x}}.",
      call = error_call
    )
  }
}

# Only x & default are translated; all other argument are evaluated in R
# See partial_call() for details
sql_recode_clauses <- function(
  ...,
  x,
  from,
  to,
  con,
  allow_empty,
  error_call
) {
  has_from <- !is_null(from)
  has_to <- !is_null(to)
  formulas <- purrr::compact(list2(...))
  has_dots <- length(formulas) > 0

  if (has_dots) {
    if (!has_from && !has_to) {
      sql_recode_clauses_formula(formulas, x, con, error_call = error_call)
    } else if (has_from) {
      cli_abort(
        "Can't supply both {.arg from} and {.arg ...}.",
        call = error_call
      )
    } else {
      cli_abort(
        "Can't supply both {.arg to} and {.arg ...}.",
        call = error_call
      )
    }
  } else {
    if (has_from && has_to) {
      sql_recode_clauses_vector(from, to, x, con, error_call = error_call)
    } else if (xor(has_from, has_to)) {
      cli_abort(
        "Must supply both {.arg from} and {.arg to}.",
        call = error_call
      )
    } else if (allow_empty) {
      character()
    } else {
      cli_abort(
        "Must supply either {.arg ...} or both {.arg from} and {.arg to}.",
        call = error_call
      )
    }
  }
}

sql_recode_clauses_formula <- function(formulas, x, con, error_call) {
  n <- length(formulas)
  clauses <- character(n)
  for (i in seq_len(n)) {
    f <- formulas[[i]]
    if (!is_formula(f, lhs = TRUE)) {
      cli_abort(
        "{.arg ...} must contain only two-sided formulas.",
        call = error_call
      )
    }
    env <- environment(f)
    query <- sql_case_match_clause(f, x, con)
    value <- escape(
      enpar(quo(!!f[[3]]), tidy = FALSE, env = env),
      con = con
    )
    clauses[[i]] <- paste0("WHEN (", query, ") THEN ", value)
  }
  clauses
}

sql_recode_clauses_vector <- function(from, to, x, con, error_call) {
  if (is.list(to)) {
    cli_abort(
      "List {.arg to} is not supported in SQL translation.",
      call = error_call
    )
  }

  n <- length(from)
  to <- vctrs::vec_recycle(to, n, x_arg = "to", call = error_call)

  clauses <- character(n)
  for (i in seq_len(n)) {
    query <- sql_recode_in_clause(from[[i]], x, con)
    value <- escape(to[[i]], con = con)
    clauses[[i]] <- paste0("WHEN (", query, ") THEN ", value)
  }
  clauses
}

sql_recode_in_clause <- function(from_values, x, con) {
  is_na <- is.na(from_values)
  non_na <- from_values[!is_na]

  query <- c(
    if (length(non_na) > 0) sql_glue("{x} IN {non_na*}"),
    if (any(is_na)) sql_glue2(con, "{x} IS NULL")
  )
  paste0(query, collapse = " OR ")
}

sql_case_finalize <- function(clauses) {
  same_line_sql <- sql(paste0("CASE ", paste0(clauses, collapse = " "), " END"))
  if (nchar(same_line_sql) <= 80) {
    return(same_line_sql)
  }
  sql(paste0("CASE\n", paste0(clauses, collapse = "\n"), "\nEND"))
}


sql_if <- function(cond, if_true, if_false = quo(NULL), missing = quo(NULL)) {
  enpared_cond <- enpar(cond)
  enpared_if_true <- enpar(if_true)
  enpared_if_false <- enpar(if_false)
  enpared_missing <- enpar(missing)
  con <- sql_current_con()

  out <- "CASE WHEN {enpared_cond} THEN {enpared_if_true}"

  # `ifelse()` and `if_else()` have a three value logic: they return `NA` resp.
  # `missing` if `cond` is `NA`. To get the same in SQL it is necessary to
  # translate to
  # CASE
  #   WHEN <cond> THEN `if_true`
  #   WHEN NOT <cond> THEN `if_false`
  #   ELSE `missing`
  # END
  if (!quo_is_null(if_false) && !identical(if_false, missing)) {
    out <- paste0(out, " WHEN NOT {enpared_cond} THEN {enpared_if_false}")
  }
  if (!quo_is_null(missing)) {
    out <- paste0(out, " ELSE {enpared_missing}")
  }
  out <- paste0(out, " END")

  sql_glue2(con, out)
}

sql_case_when <- function(
  ...,
  .default = NULL,
  .ptype = NULL,
  .size = NULL,
  error_call = caller_env()
) {
  # TODO: switch to dplyr::case_when_prepare when available
  check_unsupported_arg(.ptype, call = error_call)
  check_unsupported_arg(.size, call = error_call)
  con <- sql_current_con()

  formulas <- list2(...)
  n <- length(formulas)

  if (n == 0) {
    cli_abort("No cases provided")
  }

  query <- vector("list", n)
  value <- vector("list", n)

  for (i in seq_len(n)) {
    f <- formulas[[i]]

    env <- environment(f)
    query[[i]] <- escape(
      enpar(quo(!!f[[2]]), tidy = FALSE, env = env),
      con = con
    )
    value[[i]] <- escape(
      enpar(quo(!!f[[3]]), tidy = FALSE, env = env),
      con = con
    )
  }

  clauses <- purrr::map2_chr(query, value, function(cond, val) {
    paste0("WHEN ", cond, " THEN ", val)
  })
  # if a formula like TRUE ~ "other" is at the end of a sequence, use ELSE statement
  # TRUE has precedence over `.default`
  if (is_true(formulas[[n]][[2]])) {
    clauses[[n]] <- paste0("ELSE ", value[[n]])
  } else if (!is_null(.default)) {
    .default <- escape(enpar(quo(.default), tidy = FALSE, env = env), con = con)
    clauses[[n + 1]] <- paste0("ELSE ", .default)
  }

  sql_case_finalize(clauses)
}

sql_switch <- function(x, ...) {
  con <- sql_current_con()
  input <- list2(...)

  named <- names2(input) != ""

  clauses <- purrr::map2_chr(names(input)[named], input[named], function(x, y) {
    sql_glue2(con, "WHEN ({x}) THEN ({y})")
  })

  n_unnamed <- sum(!named)
  if (n_unnamed == 0) {
    # do nothing
  } else if (n_unnamed == 1) {
    idx <- which(!named)
    clauses <- c(clauses, sql_glue2(con, "ELSE ({input[[idx]]})"))
  } else {
    cli_abort("Can only have one unnamed (ELSE) input")
  }

  clauses_collapsed <- paste0(clauses, collapse = " ")
  sql_glue2(con, "CASE {x} {.sql clauses_collapsed} END")
}

sql_is_null <- function(x) {
  x_sql <- enpar(enquo(x))
  sql_glue("({x_sql} IS NULL)")
}

enpar <- function(x, tidy = TRUE, env = NULL) {
  if (!is_quosure(x)) {
    cli_abort("Internal error: `x` must be a quosure.") # nocov
  }

  if (tidy) {
    x_sql <- eval_tidy(x, env = env)
  } else {
    x_sql <- eval_bare(x, env = env)
  }
  if (quo_is_call(x)) {
    sql_glue("({x_sql})")
  } else {
    x_sql
  }
}
