test_that("custom scalar translated correctly", {
  local_con(simulate_snowflake())
  expect_equal(translate_sql(log10(x)), sql("LOG(10.0, `x`)"))
  expect_equal(translate_sql(round(x, digits = 1.1)), sql("ROUND((`x`) :: FLOAT, 1)"))
  expect_equal(translate_sql(grepl("exp", x)),        sql("(`x`) REGEXP ('.*' || '(exp)' || '.*')"))
  expect_error(translate_sql(grepl("exp", x, ignore.case = TRUE)),
               "`perl`, `fixed`, `useBytes`, and `ignore.case` parameters are unsupported.")
})

test_that("pasting translated correctly", {
  local_con(simulate_snowflake())

  expect_equal(translate_sql(paste(x, y)),  sql("ARRAY_TO_STRING(ARRAY_CONSTRUCT_COMPACT(`x`, `y`), ' ')"))
  expect_equal(translate_sql(paste0(x, y)), sql("ARRAY_TO_STRING(ARRAY_CONSTRUCT_COMPACT(`x`, `y`), '')"))
  expect_equal(translate_sql(str_c(x, y)), sql("CONCAT_WS('', `x`, `y`)"))
  expect_equal(translate_sql(str_c(x, y, sep = '|')), sql("CONCAT_WS('|', `x`, `y`)"))

  expect_error(translate_sql(paste0(x, collapse = "")), "`collapse` not supported")

  expect_error(translate_sql(str_flatten(x)), 'argument "collapse" is missing, with no default')
  expect_equal(translate_sql(str_flatten(x, collapse = "|"), window = TRUE), sql("LISTAGG(`x`, '|') OVER ()"))
  expect_equal(translate_sql(str_flatten(x, collapse = "|"), window = FALSE), sql("LISTAGG(`x`, '|')"))
})

test_that("custom stringr functions translated correctly", {
  local_con(simulate_snowflake())

  expect_equal(translate_sql(str_locate(x, y)), sql("POSITION(`y`, `x`)"))
  expect_equal(translate_sql(str_detect(x, y)), sql("(`x`) REGEXP ('.*' || `y` || '.*')"))
  expect_equal(translate_sql(str_detect(x, y, negate = TRUE)), sql("!((`x`) REGEXP ('.*' || `y` || '.*'))"))
  expect_equal(translate_sql(str_replace(x, y, z)), sql("REGEXP_REPLACE(`x`, `y`, `z`, 1.0, 1.0)"))
  expect_equal(translate_sql(str_replace(x, "\\d", z)), sql("REGEXP_REPLACE(`x`, '\\\\d', `z`, 1.0, 1.0)"))
  expect_equal(translate_sql(str_replace_all(x, y, z)), sql("REGEXP_REPLACE(`x`, `y`, `z`)"))
  expect_equal(translate_sql(str_squish(x)), sql("REGEXP_REPLACE(TRIM(`x`), '\\\\s+', ' ')"))
  expect_equal(translate_sql(str_remove(x, y)), sql("REGEXP_REPLACE(`x`, `y`, '', 1.0, 1.0)"))
  expect_equal(translate_sql(str_remove_all(x, y)), sql("REGEXP_REPLACE(`x`, `y`)"))
  expect_equal(translate_sql(str_trim(x)), sql("TRIM(`x`)"))
})

test_that("aggregates are translated correctly", {
  local_con(simulate_snowflake())

  expect_equal(translate_sql(cor(x, y), window = FALSE), sql("CORR(`x`, `y`)"))
  expect_equal(translate_sql(cor(x, y), window = TRUE),  sql("CORR(`x`, `y`) OVER ()"))

  expect_equal(translate_sql(cov(x, y), window = FALSE), sql("COVAR_SAMP(`x`, `y`)"))
  expect_equal(translate_sql(cov(x, y), window = TRUE),  sql("COVAR_SAMP(`x`, `y`) OVER ()"))

  expect_equal(translate_sql(all(x, na.rm = TRUE), window = FALSE), sql("BOOLAND_AGG(`x`)"))
  expect_equal(translate_sql(all(x, na.rm = TRUE), window = TRUE),  sql("BOOLAND_AGG(`x`) OVER ()"))

  expect_equal(translate_sql(any(x, na.rm = TRUE), window = FALSE), sql("BOOLOR_AGG(`x`)"))
  expect_equal(translate_sql(any(x, na.rm = TRUE), window = TRUE),  sql("BOOLOR_AGG(`x`) OVER ()"))

  expect_equal(translate_sql(sd(x, na.rm = TRUE), window = FALSE), sql("STDDEV(`x`)"))
  expect_equal(translate_sql(sd(x, na.rm = TRUE), window = TRUE),  sql("STDDEV(`x`) OVER ()"))
})

test_that("snowflake mimics two argument log", {
  local_con(simulate_snowflake())

  expect_equal(translate_sql(log(x)), sql('LN(`x`)'))
  expect_equal(translate_sql(log(x, 10)), sql('LOG(10.0, `x`)'))
  expect_equal(translate_sql(log(x, 10L)), sql('LOG(10, `x`)'))
})

test_that("custom lubridate functions translated correctly", {
  local_con(simulate_snowflake())

  expect_equal(translate_sql(day(x)),  sql("EXTRACT(DAY FROM `x`)"))
  expect_equal(translate_sql(mday(x)), sql("EXTRACT(DAY FROM `x`)"))
  expect_equal(translate_sql(yday(x)), sql("EXTRACT('dayofyear', `x`)"))
  expect_equal(translate_sql(wday(x)), sql("EXTRACT('dayofweek', DATE(`x`) + 0) + 1.0"))
  expect_equal(translate_sql(wday(x, label = TRUE)), sql("DAYNAME(`x`)"))
  expect_equal(translate_sql(wday(x, label = TRUE, abbr = FALSE)), sql(
    "DECODE(EXTRACT('dayofweek', `x`), 1.0, 'Monday', 2.0, 'Tuesday', 3.0, 'Wednesday', 4.0, 'Thursday', 5.0, 'Friday', 6.0, 'Saturday', 0.0, 'Sunday')"
  ))
  expect_equal(translate_sql(week(x)), sql("FLOOR((EXTRACT('dayofyear', `x`) - 1) / 7) + 1"))
  expect_equal(translate_sql(isoweek(x)), sql("EXTRACT('weekiso', `x`)"))
  expect_equal(translate_sql(month(x)), sql("EXTRACT('month', `x`)"))
  expect_equal(translate_sql(month(x, label = TRUE)), sql("MONTHNAME(`x`)"))
  expect_equal(translate_sql(month(x, label = TRUE, abbr = FALSE)), sql(
    "DECODE(EXTRACT('month', `x`), 1.0, 'January', 2.0, 'February', 3.0, 'March', 4.0, 'April', 5.0, 'May', 6.0, 'June', 7.0, 'July', 8.0, 'August', 9.0, 'September', 10.0, 'October', 11.0, 'November', 12.0, 'December')"
  ))
  expect_equal(translate_sql(quarter(x)), sql("EXTRACT('quarter', `x`)"))
  expect_equal(translate_sql(quarter(x, with_year = TRUE)), sql("(EXTRACT('year', `x`) || '.' || EXTRACT('quarter', `x`))"))
  expect_error(translate_sql(quarter(x, fiscal_start = 2)))
  expect_equal(translate_sql(isoyear(x)), sql("EXTRACT('year', `x`)"))

  expect_equal(translate_sql(seconds(x)), sql("INTERVAL '`x` second'"))
  expect_equal(translate_sql(minutes(x)), sql("INTERVAL '`x` minute'"))
  expect_equal(translate_sql(hours(x)),   sql("INTERVAL '`x` hour'"))
  expect_equal(translate_sql(days(x)),    sql("INTERVAL '`x` day'"))
  expect_equal(translate_sql(weeks(x)),   sql("INTERVAL '`x` week'"))
  expect_equal(translate_sql(months(x)),  sql("INTERVAL '`x` month'"))
  expect_equal(translate_sql(years(x)),   sql("INTERVAL '`x` year'"))

  expect_equal(translate_sql(floor_date(x, 'month')), sql("DATE_TRUNC('month', `x`)"))
  expect_equal(translate_sql(floor_date(x, 'week')),  sql("DATE_TRUNC('week', `x`)"))
})
