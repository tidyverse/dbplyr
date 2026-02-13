expect_translation <- function(con, expr, expected, ...) {
  expected <- paste0(expected, collapse = "\n")

  expr <- list(enexpr(expr))
  actual_sql <- translate_sql_(expr, con = con, ...)
  expect_equal(actual_sql, sql(expected))
}

expect_selects <- function(code, n) {
  expect_equal(n_selects(code), n)
}

expect_translation_snapshot <- function(con, expr, ..., error = FALSE) {
  expr <- substitute(expr)
  dots <- exprs(...)

  inject(expect_snapshot(
    translate_sql(!!expr, !!!dots, con = con),
    error = error
  ))
}

scrub_sqlite_version <- \(x) gsub(sqlite_version(), "<version>", x)
