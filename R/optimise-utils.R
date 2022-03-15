uses_mutated_vars <- function(dots, select) {
  if (is_null(select)) {
    return(TRUE)
  }

  used_vars <- get_used_vars(dots, select$name)
  ids <- vctrs::vec_match(used_vars, select$name)
  trivial <- purrr::map2_lgl(syms(select$name), select$expr, identical)[ids]

  !all(trivial)
}

get_expr_vars <- function(dots) {
  used_vars <- character()

  for (i in seq_along(dots)) {
    cur_used_vars <- all_names(get_expr(dots[[i]]))
    used_vars <- c(used_vars, cur_used_vars)
  }

  unique(used_vars)
}

expr_has_sql <- function(x) {
  if (is.sql(x)) return(TRUE)
  if (is_call(x, "sql")) return(TRUE)
  if (is_quosure(x)) return(expr_has_sql(quo_get_expr(x)))
  if (!is.call(x)) return(FALSE)

  any(purrr::map_lgl(x[-1], expr_has_sql))
}

get_used_vars <- function(dots, all_vars) {
  # if SQL is used, we don't know which variables are actually used
  # -> we simply assume every variable is used
  has_sql <- any(purrr::map_lgl(dots, expr_has_sql))
  if (has_sql) return(all_vars)

  used_vars <- character()
  for (i in seq_along(dots)) {
    cur_used_vars <- all_names(get_expr(dots[[i]]))
    used_vars <- c(used_vars, cur_used_vars)
  }

  unique(used_vars)
}
