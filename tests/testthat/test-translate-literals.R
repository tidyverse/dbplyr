context("translate-literals")


test_that("SQLite logical translation is to integers", {
  translate_sql_sqlite <- function(x) {
    translate_sql(!! enquo(x), con = simulate_sqlite())
  }

  expect_equal(translate_sql_sqlite(FALSE), sql("0"))
  expect_equal(translate_sql_sqlite(TRUE), sql("1"))
  expect_equal(translate_sql_sqlite(NA), sql("NULL"))
})
