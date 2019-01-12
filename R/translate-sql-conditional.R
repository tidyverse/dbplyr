sql_if <- function(cond, if_true, if_false = NULL) {
  build_sql(
    "CASE WHEN (", cond, ")", " THEN (", if_true, ")",
    if (!is.null(if_false))
      build_sql(" WHEN NOT(", cond, ") THEN (", if_false, ")"),
    " END"
  )
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

  old <- sql_current_context()
  on.exit(set_current_context(old), add = TRUE)
  set_current_context(list(clause = ""))
  for (i in seq_len(n)) {
    f <- formulas[[i]]

    env <- environment(f)
    query[[i]] <- escape(eval_bare(f[[2]], env), con = sql_current_con())
    value[[i]] <- escape(eval_bare(f[[3]], env), con = sql_current_con())
  }

  clauses <- purrr::map2_chr(query, value, ~ paste0("WHEN (", .x, ") THEN (", .y, ")"))
  # if a formula like TRUE ~ "other" is at the end of a sequence, use ELSE statement
  if (query[[n]] == "TRUE") {
    clauses[[n]] <- paste0("ELSE (", value[[n]], ")")
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
  sql_expr((((!!x)) %is% NULL))
}
