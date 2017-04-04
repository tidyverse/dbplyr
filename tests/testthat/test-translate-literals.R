context("translate-literals")


test_that("default logical translation is SQL 99", {
  expect_equal(translate_sql(FALSE), sql("FALSE"))
  expect_equal(translate_sql(TRUE), sql("TRUE"))
  expect_equal(translate_sql(NA), sql("NULL"))
})

test_that("SQLite logical translation is to integers", {
  translate_sql_sqlite <- function(x) {
    translate_sql(!! enquo(x), con = simulate_sqlite())
  }

  expect_equal(translate_sql_sqlite(FALSE), sql("0"))
  expect_equal(translate_sql_sqlite(TRUE), sql("1"))
  expect_equal(translate_sql_sqlite(NA), sql("NULL"))
})
