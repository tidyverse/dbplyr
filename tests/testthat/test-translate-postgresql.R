context("translate-postgresql")

# odbc base_scalar conversions -----------------------------------------
test_that("round() coreces first arg to numeric and second to integer", {
  expect_equivalent(
    translate_sql(round(x, digits = 1.1), con = simulate_odbc_postgresql()),
    sql("ROUND((`x`) :: numeric, 1)")
  )
})

test_that("log() translates to ln ", {
  expect_equivalent(
    translate_sql(log(field_name), con = simulate_odbc_postgresql()),
    sql("LN(`field_name`)")
  )
})

test_that("log() with a different base translates to formula ", {
  expect_equivalent(
    translate_sql(log(field_name, 2) , con = simulate_odbc_postgresql()),
    sql("LOG(`field_name`) / LOG(2.0)")
  )
})


test_that("two variable aggregates are translated correctly", {
  trans <- function(x, window) {
    translate_sql(!!enquo(x), window = window, con = simulate_odbc_postgresql())
  }

  expect_equal(trans(cor(x, y), window = FALSE), sql("CORR(`x`, `y`)"))
  expect_equal(trans(cor(x, y), window = TRUE),  sql("CORR(`x`, `y`) OVER ()"))
})

test_that("pasting translated correctly", {
  trans <- function(x) {
    translate_sql(!!enquo(x), window = FALSE, con = simulate_odbc_postgresql())
  }

  expect_equal(trans(paste(x, y)),  sql("CONCAT_WS(' ', `x`, `y`)"))
  expect_equal(trans(paste0(x, y)), sql("CONCAT_WS('', `x`, `y`)"))
  expect_error(trans(paste0(x, collapse = "")), "`collapse` not supported")
})
