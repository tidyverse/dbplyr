context("translate-odbc")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_odbc("OdbcConnection"))
  }

  expect_equal(trans(as.numeric(x)),            sql("CAST(`x` AS DOUBLE)"))
  expect_equal(trans(as.double(x)),             sql("CAST(`x` AS DOUBLE)"))
  expect_equal(trans(as.integer(x)),            sql("CAST(`x` AS INT)"))
  expect_equal(trans(as.logical(x)),            sql("CAST(`x` AS BOOLEAN)"))
  expect_equal(trans(as.character(x)),          sql("CAST(`x` AS STRING)"))
  expect_equal(trans(as.Date(x)),               sql("CAST(`x` AS DATE)"))
  expect_equal(trans(paste0(x, y)),             sql("CONCAT(`x`, `y`)"))
  expect_equal(trans(cosh(x)),                  sql("(EXP(`x`) + EXP(-(`x`))) / 2"))
  expect_equal(trans(sinh(x)),                  sql("(EXP(`x`) - EXP(-(`x`))) / 2"))
  expect_equal(trans(tanh(x)),                  sql("((EXP(`x`) - EXP(-(`x`))) / 2) / ((EXP(`x`) + EXP(-(`x`))) / 2)"))
  expect_equal(trans(round(10.1)),              sql("ROUND(10.1, 0)"))
  expect_equal(trans(round(10.1, digits = 1)),  sql("ROUND(10.1, 1)"))
  expect_equal(trans(coth(x)),                  sql("((EXP(`x`) + EXP(-(`x`))) / 2) / ((EXP(`x`) - EXP(-(`x`))) / 2)"))
  expect_equal(trans(paste(x, y)),              sql("CONCAT_WS(' ', `x`,`y`)"))
  expect_equal(trans(paste(x, y, sep = ",")),   sql("CONCAT_WS(',', `x`,`y`)"))

})

test_that("custom aggregators translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = FALSE, con = simulate_odbc("OdbcConnection"))
  }

  expect_equal(trans(sd(x)),         sql("STDDEV_SAMP(`x`)"))
  expect_equal(trans(count()),       sql("COUNT(*)"))
  expect_equal(trans(n()),           sql("COUNT(*)"))
  expect_equal(trans(n_distinct(x)), sql("COUNT(DISTINCT `x`)"))


})

test_that("custom window functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = TRUE, con = simulate_odbc("OdbcConnection"))
  }

  expect_equal(trans(sd(x, na.rm = TRUE)),         sql("STDDEV_SAMP(`x`) OVER ()"))
  expect_equal(trans(count()),       sql("COUNT(*) OVER ()"))
  expect_equal(trans(n()),           sql("COUNT(*) OVER ()"))
  expect_equal(trans(n_distinct(x)), sql("COUNT(DISTINCT `x`) OVER ()"))


})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, src = simulate_odbc("OdbcConnection"))

  expect_equal(
    mf %>% tally() %>% show_query(),
    sql("SELECT COUNT(*) AS `n`\nFROM `df`")
  )

  expect_equal(
    mf %>% summarise(count = n()) %>% show_query(),
    sql("SELECT COUNT(*) AS `count`\nFROM `df`")
  )
})


