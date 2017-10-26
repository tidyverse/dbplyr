context("translate-postgresql")

# odbc base_scalar conversions -----------------------------------------
test_that("round() coreces first arg to numeric and second to integer", {
  expect_equivalent(
    translate_sql(round(x, digits = 1.1), con = simulate_odbc_postgresql()),
    sql("ROUND((`x`)::numeric, 1)")
  )
})

test_that("cosh() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      cosh(field1),
      con = simulate_odbc_postgresql()),
    sql("(EXP(`field1`) + EXP(-`field1`)) / 2"))
})
test_that("sinh() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      sinh(field1),
      con = simulate_odbc_postgresql()),
    sql("(EXP(`field1`) - EXP(-`field1`)) / 2"))
})
test_that("tanh() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      tanh(field1),
      con = simulate_odbc_postgresql()),
    sql("((EXP(`field1`) - EXP(-`field1`)) / 2) / ((EXP(`field1`) + EXP(-`field1`)) / 2)"))
})
test_that("coth() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      coth(field1),
      con = simulate_odbc_postgresql()),
    sql("((EXP(`field1`) + EXP(-`field1`)) / 2) / ((EXP(`field1`) - EXP(-`field1`)) / 2)"))
})

test_that("log() translates to ln ", {
  expect_equivalent(
    translate_sql(log(field_name), con = simulate_odbc_postgresql()),
    sql("ln(`field_name`)")
  )
})

test_that("log() with a different base translates to formula ", {
  expect_equivalent(
    translate_sql(log(field_name, 2) , con = simulate_odbc_postgresql()),
    sql("log(`field_name`) / log(2.0)")
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

  expect_equal(trans(paste(x, y)),  sql("CONCAT_WS(' ', `x`,`y`)"))
  expect_equal(trans(paste0(x, y)), sql("CONCAT_WS('', `x`,`y`)"))
  expect_error(trans(paste0(x, collapse = "")), "`collapse` not supported")
})
