test_that("custom scalar translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(bitwXor(x, 128L)),       sql("`x` # 128"))
  expect_equal(translate_sql(log10(x)),               sql("LOG(`x`)"))
  expect_equal(translate_sql(log(x)),                 sql("LN(`x`)"))
  expect_equal(translate_sql(log(x, 2)),              sql("LOG(`x`) / LOG(2.0)"))
  expect_equal(translate_sql(cot(x)),                 sql("1 / TAN(`x`)"))
  expect_equal(translate_sql(round(x, digits = 1.1)), sql("ROUND((`x`) :: numeric, 1)"))
  expect_equal(translate_sql(grepl("exp", x)),        sql("(`x`) ~ ('exp')"))
  expect_equal(translate_sql(grepl("exp", x, TRUE)),  sql("(`x`) ~* ('exp')"))
  expect_equal(translate_sql(substr("test", 2 , 3)),  sql("SUBSTR('test', 2, 2)"))
})

test_that("custom stringr functions translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(str_detect(x, y)), sql("`x` ~ `y`"))
  expect_equal(translate_sql(str_detect(x, y, negate = TRUE)), sql("!(`x` ~ `y`)"))
  expect_equal(translate_sql(str_replace(x, y, z)), sql("REGEXP_REPLACE(`x`, `y`, `z`)"))
  expect_equal(translate_sql(str_replace_all(x, y, z)), sql("REGEXP_REPLACE(`x`, `y`, `z`, 'g')"))
  expect_equal(translate_sql(str_squish(x)), sql("LTRIM(RTRIM(REGEXP_REPLACE(`x`, '\\s+', ' ', 'g')))"))
  expect_equal(translate_sql(str_remove(x, y)), sql("REGEXP_REPLACE(`x`, `y`, '')"))
  expect_equal(translate_sql(str_remove_all(x, y)), sql("REGEXP_REPLACE(`x`, `y`, '', 'g')"))
})

test_that("two variable aggregates are translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(cor(x, y), window = FALSE), sql("CORR(`x`, `y`)"))
  expect_equal(translate_sql(cor(x, y), window = TRUE),  sql("CORR(`x`, `y`) OVER ()"))
})

test_that("pasting translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(paste(x, y), window = FALSE),  sql("CONCAT_WS(' ', `x`, `y`)"))
  expect_equal(translate_sql(paste0(x, y), window = FALSE), sql("CONCAT_WS('', `x`, `y`)"))

  expect_error(translate_sql(paste0(x, collapse = ""), window = FALSE), "`collapse` not supported")
})

test_that("postgres mimics two argument log", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(log(x)), sql('LN(`x`)'))
  expect_equal(translate_sql(log(x, 10)), sql('LOG(`x`) / LOG(10.0)'))
  expect_equal(translate_sql(log(x, 10L)), sql('LOG(`x`) / LOG(10)'))
})

test_that("custom lubridate functions translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(yday(x)), sql("EXTRACT(DOY FROM `x`)"))
  expect_equal(translate_sql(quarter(x)), sql("EXTRACT(QUARTER FROM `x`)"))
  expect_equal(translate_sql(quarter(x, with_year = TRUE)), sql("(EXTRACT(YEAR FROM `x`) || '.' || EXTRACT(QUARTER FROM `x`))"))
  expect_error(translate_sql(quarter(x, fiscal_start = 2)))

  expect_equal(translate_sql(seconds(x)), sql("CAST('`x` seconds' AS INTERVAL)"))
  expect_equal(translate_sql(minutes(x)), sql("CAST('`x` minutes' AS INTERVAL)"))
  expect_equal(translate_sql(hours(x)),   sql("CAST('`x` hours' AS INTERVAL)"))
  expect_equal(translate_sql(days(x)),    sql("CAST('`x` days' AS INTERVAL)"))
  expect_equal(translate_sql(weeks(x)),   sql("CAST('`x` weeks' AS INTERVAL)"))
  expect_equal(translate_sql(months(x)),  sql("CAST('`x` months' AS INTERVAL)"))
  expect_equal(translate_sql(years(x)),   sql("CAST('`x` years' AS INTERVAL)"))

  expect_equal(translate_sql(floor_date(x, 'month')),       sql("DATE_TRUNC('month', `x`)"))
  expect_equal(translate_sql(floor_date(x, 'week')),        sql("DATE_TRUNC('week', `x`)"))
})

test_that("custom SQL translation", {
  lf <- lazy_frame(x = 1, con = simulate_postgres())
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))
})

# live database -----------------------------------------------------------

test_that("can explain", {
  db <- copy_to_test("postgres", data.frame(x = 1:3))
  expect_snapshot(db %>% mutate(y = x + 1) %>% explain())
})

test_that("can overwrite temp tables", {
  src <- src_test("postgres")
  copy_to(src, mtcars, "mtcars", overwrite = TRUE)
  expect_error(copy_to(src, mtcars, "mtcars", overwrite = TRUE), NA)
})
