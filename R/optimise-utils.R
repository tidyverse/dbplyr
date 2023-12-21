uses_mutated_vars <- function(dots, select) {
  if (is_null(select)) {
    return(TRUE)
  }

  vars <- set_names(select$expr, select$name)
  mutated_vars <- names(purrr::discard(vars, is_symbol))

  if (is_empty(mutated_vars)) {
    return(FALSE)
  }

  any(purrr::map_lgl(dots, expr_uses_var, mutated_vars))
}

expr_uses_var <- function(x, vars) {
  if (is.sql(x)) return(TRUE)
  if (is_call(x, "sql")) return(TRUE)
  if (is_quosure(x)) return(expr_uses_var(quo_get_expr(x), vars))
  if (is_symbol(x)) {
    return(is_symbol(x, vars))
  }

  any(purrr::map_lgl(as.list(x[-1]), expr_uses_var, vars))
}

any_expr_uses_sql <- function(dots) {
  purrr::some(dots, expr_uses_sql)
}

expr_uses_sql <- function(x) {
  if (is.sql(x)) return(TRUE)
  if (is_call(x, "sql")) return(TRUE)
  if (is_quosure(x)) return(expr_uses_sql(quo_get_expr(x)))

  if (!is_call(x)) return(FALSE)

  any(purrr::map_lgl(as.list(x[-1]), expr_uses_sql))
}
