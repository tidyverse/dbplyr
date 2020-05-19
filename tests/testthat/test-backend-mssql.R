context("test-backend-mssql.R")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_mssql())
  }

  expect_equal(trans(as.logical(x)),   sql("CAST(`x` AS BIT)"))
  expect_equal(trans(as.numeric(x)),   sql("CAST(`x` AS FLOAT)"))
  expect_equal(trans(as.double(x)),    sql("CAST(`x` AS FLOAT)"))
  expect_equal(trans(as.character(x)), sql("CAST(`x` AS VARCHAR(MAX))"))
  expect_equal(trans(log(x)),          sql("LOG(`x`)"))
  expect_equal(trans(nchar(x)),        sql("LEN(`x`)"))
  expect_equal(trans(atan2(x)),        sql("ATN2(`x`)"))
  expect_equal(trans(ceiling(x)),      sql("CEILING(`x`)"))
  expect_equal(trans(ceil(x)),         sql("CEILING(`x`)"))
  expect_equal(trans(substr(x, 1, 2)), sql("SUBSTRING(`x`, 1, 2)"))
  expect_equal(trans(trimws(x)),       sql("LTRIM(RTRIM(`x`))"))
  expect_equal(trans(paste(x, y)),     sql("`x` + ' ' + `y`"))

  expect_error(trans(bitwShiftL(x, 2L)), sql("not available"))
  expect_error(trans(bitwShiftR(x, 2L)), sql("not available"))
})

test_that("custom stringr functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_mssql())
  }

  expect_equal(trans(str_length(x)),         sql("LEN(`x`)"))

})

test_that("custom aggregators translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = FALSE, con = simulate_mssql())
  }

  expect_equal(trans(sd(x, na.rm = TRUE)),  sql("STDEV(`x`)"))
  expect_equal(trans(var(x, na.rm = TRUE)), sql("VAR(`x`)"))

  expect_error(trans(cor(x)), "not available")
  expect_error(trans(cov(x)), "not available")

  expect_equal(trans(str_flatten(x)), sql("STRING_AGG(`x`, '')"))
})

test_that("custom window functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = TRUE, con = simulate_mssql())
  }

  expect_equal(trans(sd(x, na.rm = TRUE)),  sql("STDEV(`x`) OVER ()"))
  expect_equal(trans(var(x, na.rm = TRUE)), sql("VAR(`x`) OVER ()"))

  expect_error(trans(cor(x)), "not supported")
  expect_error(trans(cov(x)), "not supported")

  expect_equal(trans(str_flatten(x)), sql("STRING_AGG(`x`, '') OVER ()"))
})

test_that("custom lubridate functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_mssql())
  }

  expect_equal(trans(as_date(x)),     sql("CAST(`x` AS DATE)"))
  expect_equal(trans(as_datetime(x)), sql("CAST(`x` AS DATETIME2)"))

  expect_equal(trans(today()),   sql("CAST(SYSDATETIME() AS DATE)"))
  expect_equal(trans(year(x)),   sql("DATEPART(YEAR, `x`)"))
  expect_equal(trans(day(x)),    sql("DATEPART(DAY, `x`)"))
  expect_equal(trans(mday(x)),   sql("DATEPART(DAY, `x`)"))
  expect_equal(trans(yday(x)),   sql("DATEPART(DAYOFYEAR, `x`)"))
  expect_equal(trans(hour(x)),   sql("DATEPART(HOUR, `x`)"))
  expect_equal(trans(minute(x)), sql("DATEPART(MINUTE, `x`)"))
  expect_equal(trans(second(x)), sql("DATEPART(SECOND, `x`)"))

  expect_equal(trans(month(x)), sql("DATEPART(MONTH, `x`)"))
  expect_equal(trans(month(x, label = TRUE, abbr = FALSE)), sql("DATENAME(MONTH, `x`)"))
  expect_error(trans(month(x, abbr = TRUE, abbr = TRUE)))

  expect_equal(trans(quarter(x)), sql("DATEPART(QUARTER, `x`)"))
  expect_equal(trans(quarter(x, with_year = TRUE)), sql("(DATENAME(YEAR, `x`) + '.' + DATENAME(QUARTER, `x`))"))
  expect_error(trans(quarter(x, fiscal_start = 5)))
})

verify_lazy_output("sql/mssql.sql", {
  mf <- lazy_frame(x = 1:3, con = simulate_mssql())

  "# filter and mutate translate is.na correctly"
  mf %>% head()

  mf %>% mutate(z = is.na(x))

  mf %>% mutate(z = !is.na(x))

  mf %>% filter(is.na(x))

  "Invalid, can we reliably detect that the return value is logical?"
  mf %>% mutate(x = x == 1)

  mf %>% mutate(x = x != 1)

  mf %>% mutate(x = x > 1)

  mf %>% mutate(x = x >= 1)

  mf %>% mutate(x = !(x == 1))

  mf %>% mutate(x = !(x != 1))

  mf %>% mutate(x = !(x > 1))

  mf %>% mutate(x = !(x >= 1))

  mf %>% mutate(x = x > 4 & x < 5)

  mf %>% filter(x > 4 & x < 5)

  mf %>% mutate(x = x > 4 | x < 5)

  mf %>% filter(x > 4 | x < 5)

  "Work around with explicit ifelse()"
  mf %>% mutate(x = ifelse(x == 0, 0, 1))

  "# Special ifelse and case_when cases return the correct queries"
  mf %>% mutate(z = ifelse(x %in% c(1, 2), 0, 1))

  mf %>% mutate(z = case_when(is.na(x) ~ 1, !is.na(x) ~ 2, TRUE ~ 3))

  "# ORDER BY in subqueries uses TOP 100 PERCENT (#175)"
  mf %>% mutate(x = -x) %>% arrange(x) %>% mutate(x = -x)
})
