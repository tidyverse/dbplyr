context("translate-postgresql")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_odbc_postgresql())
  }

  expect_equal(trans(log10(x)),               sql("LOG(`x`)"))
  expect_equal(trans(log(x)),                 sql("LN(`x`)"))
  expect_equal(trans(log(x, 2)),              sql("LOG(`x`) / LOG(2.0)"))
  expect_equal(trans(cot(x)),                 sql("1 / TAN(`x`)"))
  expect_equal(trans(round(x, digits = 1.1)), sql("ROUND((`x`) :: numeric, 1)"))
  expect_equal(trans(grepl("exp", x)),        sql("(`x`) ~ ('exp')"))
  expect_equal(trans(grepl("exp", x, TRUE)),  sql("(`x`) ~* ('exp')"))
})


test_that("custom stringr functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_odbc_postgresql())
  }

  expect_equal(trans(str_locate(x, y)), sql("STRPOS(`x`, `y`)"))
  expect_equal(trans(str_detect(x, y)), sql("STRPOS(`x`, `y`) > 0"))

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
