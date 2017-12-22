context("translate-teradata")


test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_teradata())
  }

  expect_equal(trans(x != y),          sql("`x` <> `y`"))
  expect_equal(trans(as.numeric(x)),   sql("CAST(`x` AS NUMERIC)"))
  expect_equal(trans(as.double(x)),    sql("CAST(`x` AS NUMERIC)"))
  expect_equal(trans(as.character(x)), sql("CAST(`x` AS VARCHAR(MAX))"))
  expect_equal(trans(log(x)),          sql("LN(`x`)"))
  expect_equal(trans(cot(x)),          sql("1 / TAN(`x`)"))
  expect_equal(trans(nchar(x)),        sql("CHARACTER_LENGTH(`x`)"))
  expect_equal(trans(ceil(x)),         sql("CEILING(`x`)"))
  expect_equal(trans(ceiling(x)),      sql("CEILING(`x`)"))
  expect_equal(trans(atan2(x, y)),     sql("ATAN2(`y`,`x`)"))
  expect_equal(trans(substr(x, 1, 2)), sql("SUBSTR(`x`, 1.0, 2.0)"))

  expect_error(trans(paste(x)),        sql("not supported"))

})

test_that("custom aggregators translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = FALSE, con = simulate_teradata())
  }

  expect_equal(trans(var(x)), sql("VAR_SAMP(`x`)"))

  expect_error(trans(cor(x)), "not available")
  expect_error(trans(cov(x)), "not available")
})

test_that("custom window functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = TRUE, con = simulate_teradata())
  }

  expect_equal(trans(var(x, na.rm = TRUE)), sql("VAR_SAMP(`x`) OVER ()"))

  expect_error(trans(cor(x)), "not supported")
  expect_error(trans(cov(x)), "not supported")
})

test_that("filter and mutate translate is.na correctly", {
  mf <- lazy_frame(x = 1, src = simulate_teradata())

  expect_equal(
    mf %>% head() %>% show_query(),
    sql("SELECT  TOP 6 *\nFROM `df`")
  )

})
