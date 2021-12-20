sql_if <- function(cond, if_true, if_false = NULL, missing = NULL) {
  out <- build_sql("CASE WHEN ", enpar(cond), " THEN ", enpar(if_true))

  if (!is.null(if_false) && identical(if_false, missing)) {
    out <- paste0(out, " ELSE ", enpar(if_false), " END")
    return(sql(out))
  }

  if (!is.null(if_false)) {
    false_sql <- build_sql(" WHEN NOT ", enpar(cond), " THEN ", enpar(if_false))
    out <- paste0(out, false_sql)
  }

  if (!is.null(missing)) {
    missing_cond <- translate_sql(is.na(!!cond), con = sql_current_con())
    missing_sql <- build_sql(" WHEN ", missing_cond, " THEN ", enpar(missing))
    out <- paste0(out, missing_sql)
  }

  sql(paste0(out, " END"))
}

sql_case_when <- function(...) {
  # TODO: switch to dplyr::case_when_prepare when available

  formulas <- list2(...)
  n <- length(formulas)

  if (n == 0) {
    abort("No cases provided")
  }

  query <- vector("list", n)
  value <- vector("list", n)

  for (i in seq_len(n)) {
    f <- formulas[[i]]

    env <- environment(f)
    query[[i]] <- enpar(expr(!!f[[2]]))
    value[[i]] <- enpar(expr(!!f[[3]]))
  }

  clauses <- purrr::map2_chr(query, value, ~ paste0("WHEN ", .x, " THEN ", .y))
  # if a formula like TRUE ~ "other" is at the end of a sequence, use ELSE statement
  if (query[[n]] == "TRUE") {
    clauses[[n]] <- paste0("ELSE ", value[[n]])
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

  named <- names(input) != ""

  clauses <- purrr::map2_chr(names(input)[named], input[named], function(x, y) {
    build_sql("WHEN (", x , ") THEN (", y, ") ")
  })

  n_unnamed <- sum(!named)
  if (n_unnamed == 0) {
    # do nothing
  } else if (n_unnamed == 1) {
    clauses <- c(clauses, build_sql("ELSE ", input[!named], " "))
  } else {
    stop("Can only have one unnamed (ELSE) input", call. = FALSE)
  }

  build_sql("CASE ", x, " ", !!!clauses, "END")
}

sql_is_null <- function(x) {
  x_expr <- enexpr(x)
  if (is_call(x_expr)) {
    sql_expr((((!!x)) %is% NULL))
  } else {
    sql_expr((!!x %is% NULL))
  }
}

enpar <- function(x) {
  if (is_call(x)) {
    build_sql("(", translate_sql(!!x), ")")
  } else {
    translate_sql(!!x)
  }
}
