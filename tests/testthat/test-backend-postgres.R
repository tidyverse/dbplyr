context("test-backend-postgres.R")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_postgres())
  }

  expect_equal(trans(bitwXor(x, 128L)),       sql("`x` # 128"))
  expect_equal(trans(log10(x)),               sql("LOG(`x`)"))
  expect_equal(trans(log(x)),                 sql("LN(`x`)"))
  expect_equal(trans(log(x, 2)),              sql("LOG(`x`) / LOG(2.0)"))
  expect_equal(trans(cot(x)),                 sql("1 / TAN(`x`)"))
  expect_equal(trans(round(x, digits = 1.1)), sql("ROUND((`x`) :: numeric, 1)"))
  expect_equal(trans(grepl("exp", x)),        sql("(`x`) ~ ('exp')"))
  expect_equal(trans(grepl("exp", x, TRUE)),  sql("(`x`) ~* ('exp')"))
  expect_equal(trans(substr("test", 2 , 3)),  sql("SUBSTR('test', 2, 2)"))
})


test_that("custom stringr functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_postgres())
  }
  expect_equal(trans(str_replace_all(x, y, z)), sql("REGEXP_REPLACE(`x`, `y`, `z`)"))
})

test_that("two variable aggregates are translated correctly", {
  trans <- function(x, window) {
    translate_sql(!!enquo(x), window = window, con = simulate_postgres())
  }

  expect_equal(trans(cor(x, y), window = FALSE), sql("CORR(`x`, `y`)"))
  expect_equal(trans(cor(x, y), window = TRUE),  sql("CORR(`x`, `y`) OVER ()"))

})

test_that("pasting translated correctly", {
  trans <- function(x) {
    translate_sql(!!enquo(x), window = FALSE, con = simulate_postgres())
  }

  expect_equal(trans(paste(x, y)),  sql("CONCAT_WS(' ', `x`, `y`)"))
  expect_equal(trans(paste0(x, y)), sql("CONCAT_WS('', `x`, `y`)"))

  expect_error(trans(paste0(x, collapse = "")), "`collapse` not supported")
})

test_that("postgres mimics two argument log", {
  trans <- function(...) {
    translate_sql(..., con = simulate_postgres())
  }

  expect_equal(trans(log(x)), sql('LN(`x`)'))
  expect_equal(trans(log(x, 10)), sql('LOG(`x`) / LOG(10.0)'))
  expect_equal(trans(log(x, 10L)), sql('LOG(`x`) / LOG(10)'))
})

test_that("custom lubridate functions translated correctly", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_postgres())
  }

  expect_equal(trans(yday(x)),                      sql("EXTRACT(DOY FROM `x`)"))

  expect_equal(trans(quarter(x)),                   sql("EXTRACT(QUARTER FROM `x`)"))
  expect_equal(trans(quarter(x, with_year = TRUE)), sql("(EXTRACT(YEAR FROM `x`) || '.' || EXTRACT(QUARTER FROM `x`))"))

  expect_error(trans(quarter(x, fiscal_start = 2)))
})

test_that("postgres can explain (#272)", {
  skip_if_no_db("postgres")

  df1 <- data.frame(x = 1:3)

  expect_output(expect_error(
    src_test("postgres") %>%
      copy_to(df1, unique_table_name()) %>%
      mutate(y = x + 1) %>%
      explain(),
    NA
  ))
})
