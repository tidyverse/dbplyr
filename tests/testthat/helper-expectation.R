expect_translation <- function(con, expr, expected, ...) {
  expected <- paste0(expected, collapse = "\n")
  actual_sql <- translate_sql({{ expr }}, con = con, ...)
  expect_equal(actual_sql, sql(expected))
}

expect_translation_snapshot <- function(con, expr, ..., error = FALSE) {
  expr <- substitute(expr)
  dots <- exprs(...)

  inject(expect_snapshot(
    translate_sql(!!expr, !!!dots, con = con),
    error = error
  ))
}
