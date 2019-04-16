context("test-backend-mssql.R")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_mssql())
  }

  expect_equal(trans(as.logical(x)),   sql("CAST(`x` AS BIT)"))
  expect_equal(trans(as.numeric(x)),   sql("CAST(`x` AS NUMERIC)"))
  expect_equal(trans(as.double(x)),    sql("CAST(`x` AS NUMERIC)"))
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
})

test_that("custom window functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = TRUE, con = simulate_mssql())
  }

  expect_equal(trans(sd(x, na.rm = TRUE)),  sql("STDEV(`x`) OVER ()"))
  expect_equal(trans(var(x, na.rm = TRUE)), sql("VAR(`x`) OVER ()"))

  expect_error(trans(cor(x)), "not supported")
  expect_error(trans(cov(x)), "not supported")
})

test_that("filter and mutate translate is.na correctly", {
  mf <- lazy_frame(x = 1, con = simulate_mssql())

  expect_equal(
    mf %>% head() %>% sql_render(),
    sql("SELECT TOP(6) *\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(z = is.na(x)) %>% sql_render(),
    sql("SELECT `x`, CONVERT(BIT, IIF(`x` IS NULL, 1, 0)) AS `z`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(z = !is.na(x)) %>% sql_render(),
    sql("SELECT `x`, ~(CONVERT(BIT, IIF(`x` IS NULL, 1, 0))) AS `z`\nFROM `df`")
  )

  expect_equal(
    mf %>% filter(is.na(x)) %>% sql_render(),
    sql("SELECT *\nFROM `df`\nWHERE (((`x`) IS NULL))")
  )

  expect_equal(
    mf %>% mutate(x = x == 1) %>% sql_render(),
    sql("SELECT `x` = 1.0 AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = x != 1) %>% sql_render(),
    sql("SELECT `x` != 1.0 AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = x > 1) %>% sql_render(),
    sql("SELECT `x` > 1.0 AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = x >= 1) %>% sql_render(),
    sql("SELECT `x` >= 1.0 AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = !(x == 1)) %>% sql_render(),
    sql("SELECT ~((`x` = 1.0)) AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = !(x != 1)) %>% sql_render(),
    sql("SELECT ~((`x` != 1.0)) AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = !(x > 1)) %>% sql_render(),
    sql("SELECT ~((`x` > 1.0)) AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = !(x >= 1)) %>% sql_render(),
    sql("SELECT ~((`x` >= 1.0)) AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% mutate(x = x > 4 & x < 5) %>% sql_render(),
    sql("SELECT `x` > 4.0 & `x` < 5.0 AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% filter(x > 4 & x < 5) %>% sql_render(),
    sql("SELECT *\nFROM `df`\nWHERE (`x` > 4.0 AND `x` < 5.0)")
  )

  expect_equal(
    mf %>% mutate(x = x > 4 | x < 5) %>% sql_render(),
    sql("SELECT `x` > 4.0 | `x` < 5.0 AS `x`\nFROM `df`")
  )

  expect_equal(
    mf %>% filter(x > 4 | x < 5) %>% sql_render(),
    sql("SELECT *\nFROM `df`\nWHERE (`x` > 4.0 OR `x` < 5.0)")
  )

  expect_equal(
    mf %>% mutate(x = ifelse(x == 0, 0 ,1)) %>% sql_render(),
    sql("SELECT CASE WHEN (`x` = 0.0) THEN (0.0) WHEN NOT(`x` = 0.0) THEN (1.0) END AS `x`\nFROM `df`")
  )
})

test_that("Special ifelse and case_when cases return the correct queries", {
  mf <- lazy_frame(x = 1, con = simulate_mssql())
  expect_equal(
    mf %>% mutate(z = ifelse(x %in% c(1, 2), 0, 1)) %>% sql_render(),
    sql("SELECT `x`, CASE WHEN (`x` IN (1.0, 2.0)) THEN (0.0) WHEN NOT(`x` IN (1.0, 2.0)) THEN (1.0) END AS `z`
FROM `df`")
  )
  expect_equal(
    mf %>% mutate(z = case_when(is.na(x) ~ 1, !is.na(x) ~ 2, TRUE ~ 3)) %>% sql_render(),
    sql("SELECT `x`, CASE\nWHEN (((`x`) IS NULL)) THEN (1.0)\nWHEN (NOT(((`x`) IS NULL))) THEN (2.0)\nELSE (3.0)\nEND AS `z`\nFROM `df`")
  )
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
