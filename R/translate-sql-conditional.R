sql_if <- function(cond, if_true, if_false = quo(NULL), missing = quo(NULL)) {
  out <- build_sql("CASE WHEN ", enpar(cond), " THEN ", enpar(if_true))

  # `ifelse()` and `if_else()` have a three value logic: they return `NA` resp.
  # `missing` if `cond` is `NA`. To get the same in SQL it is necessary to
  # translate to
  # CASE
  #   WHEN <cond> THEN `if_true`
  #   WHEN NOT <cond> THEN `if_false`
  #   WHEN <cond> IS NULL THEN `missing`
  # END
  #
  # Together these cases cover every possible case. So, if `if_false` and
  # `missing` are identical they can be simplified to `ELSE <if_false>`
  if (!quo_is_null(if_false) && identical(if_false, missing)) {
    out <- paste0(out, " ELSE ", enpar(if_false), " END")
    return(sql(out))
  }

  if (!quo_is_null(if_false)) {
    false_sql <- build_sql(" WHEN NOT ", enpar(cond), " THEN ", enpar(if_false))
    out <- paste0(out, false_sql)
  }

  if (!quo_is_null(missing)) {
    missing_cond <- translate_sql(is.na(!!cond), con = sql_current_con())
    missing_sql <- build_sql(" WHEN ", missing_cond, " THEN ", enpar(missing))
    out <- paste0(out, missing_sql)
  }

  sql(paste0(out, " END"))
}

sql_case_when <- function(...,
                          .default = NULL,
                          .ptype = NULL,
                          .size = NULL,
                          error_call = caller_env()) {
  # TODO: switch to dplyr::case_when_prepare when available
  check_not_supplied(.ptype, call = error_call)
  check_not_supplied(.size, call = error_call)

  # browser()
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
    query[[i]] <- escape(enpar(quo(!!f[[2]]), tidy = FALSE, env = env), con = sql_current_con())
    value[[i]] <- escape(enpar(quo(!!f[[3]]), tidy = FALSE, env = env), con = sql_current_con())
  }

  clauses <- purrr::map2_chr(query, value, ~ paste0("WHEN ", .x, " THEN ", .y))
  # if a formula like TRUE ~ "other" is at the end of a sequence, use ELSE statement
  # TRUE has precedence over `.default`
  if (is_true(formulas[[n]][[2]])) {
    clauses[[n]] <- paste0("ELSE ", value[[n]])
  } else if (!is_null(.default)) {
    .default <- escape(enpar(quo(.default), tidy = FALSE, env = env), con = sql_current_con())
    clauses[[n + 1]] <- paste0("ELSE ", .default)
  }

  same_line_sql <- sql(paste0("CASE ", paste0(clauses, collapse = " "), " END"))
  if (nchar(same_line_sql) <= 80) {
    return(same_line_sql)
  }

  sql(paste0(
    "CASE\n",
    paste0(clauses, collapse = "\n"),
    "\nEND"
  ))
}

sql_switch <- function(x, ...) {
  input <- list2(...)

  named <- names2(input) != ""

  clauses <- purrr::map2_chr(names(input)[named], input[named], function(x, y) {
    build_sql("WHEN (", x , ") THEN (", y, ") ")
  })

  n_unnamed <- sum(!named)
  if (n_unnamed == 0) {
    # do nothing
  } else if (n_unnamed == 1) {
    clauses <- c(clauses, build_sql("ELSE ", input[!named], " "))
  } else {
    cli_abort("Can only have one unnamed (ELSE) input")
  }

  build_sql("CASE ", x, " ", !!!clauses, "END")
}

sql_is_null <- function(x) {
  x_sql <- enpar(enquo(x))
  sql_expr((!!x_sql %is% NULL))
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
    build_sql("(", x_sql, ")")
  } else {
    x_sql
  }
}
