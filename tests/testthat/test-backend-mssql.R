context("test-backend-mssql.R")


# function translation ----------------------------------------------------

test_that("custom scalar translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(as.logical(x)),   sql("CAST(`x` AS BIT)"))
  expect_equal(translate_sql(as.numeric(x)),   sql("CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(as.double(x)),    sql("CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(as.character(x)), sql("CAST(`x` AS VARCHAR(MAX))"))
  expect_equal(translate_sql(log(x)),          sql("LOG(`x`)"))
  expect_equal(translate_sql(nchar(x)),        sql("LEN(`x`)"))
  expect_equal(translate_sql(atan2(x)),        sql("ATN2(`x`)"))
  expect_equal(translate_sql(ceiling(x)),      sql("CEILING(`x`)"))
  expect_equal(translate_sql(ceil(x)),         sql("CEILING(`x`)"))
  expect_equal(translate_sql(substr(x, 1, 2)), sql("SUBSTRING(`x`, 1, 2)"))
  expect_equal(translate_sql(trimws(x)),       sql("LTRIM(RTRIM(`x`))"))
  expect_equal(translate_sql(paste(x, y)),     sql("`x` + ' ' + `y`"))

  expect_error(translate_sql(bitwShiftL(x, 2L)), sql("not available"))
  expect_error(translate_sql(bitwShiftR(x, 2L)), sql("not available"))
})

test_that("custom stringr functions translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(str_length(x)),     sql("LEN(`x`)"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(sd(x, na.rm = TRUE), window = FALSE),  sql("STDEV(`x`)"))
  expect_equal(translate_sql(var(x, na.rm = TRUE), window = FALSE), sql("VAR(`x`)"))

  expect_error(translate_sql(cor(x), window = FALSE), "not available")
  expect_error(translate_sql(cov(x), window = FALSE), "not available")

  expect_equal(translate_sql(str_flatten(x), window = FALSE), sql("STRING_AGG(`x`, '')"))
})

test_that("custom window functions translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(sd(x, na.rm = TRUE)),  sql("STDEV(`x`) OVER ()"))
  expect_equal(translate_sql(var(x, na.rm = TRUE)), sql("VAR(`x`) OVER ()"))

  expect_error(translate_sql(cor(x)), "not supported")
  expect_error(translate_sql(cov(x)), "not supported")

  expect_equal(translate_sql(str_flatten(x)), sql("STRING_AGG(`x`, '') OVER ()"))
})

test_that("custom lubridate functions translated correctly", {
  local_con(simulate_mssql())
  expect_equal(translate_sql(as_date(x)),     sql("CAST(`x` AS DATE)"))
  expect_equal(translate_sql(as_datetime(x)), sql("CAST(`x` AS DATETIME2)"))
  expect_equal(translate_sql(today()),   sql("CAST(SYSDATETIME() AS DATE)"))
  expect_equal(translate_sql(year(x)),   sql("DATEPART(YEAR, `x`)"))
  expect_equal(translate_sql(day(x)),    sql("DATEPART(DAY, `x`)"))
  expect_equal(translate_sql(mday(x)),   sql("DATEPART(DAY, `x`)"))
  expect_equal(translate_sql(yday(x)),   sql("DATEPART(DAYOFYEAR, `x`)"))
  expect_equal(translate_sql(hour(x)),   sql("DATEPART(HOUR, `x`)"))
  expect_equal(translate_sql(minute(x)), sql("DATEPART(MINUTE, `x`)"))
  expect_equal(translate_sql(second(x)), sql("DATEPART(SECOND, `x`)"))
  expect_equal(translate_sql(month(x)), sql("DATEPART(MONTH, `x`)"))
  expect_equal(translate_sql(month(x, label = TRUE, abbr = FALSE)), sql("DATENAME(MONTH, `x`)"))
  expect_error(translate_sql(month(x, abbr = TRUE, abbr = TRUE)))

  expect_equal(translate_sql(quarter(x)), sql("DATEPART(QUARTER, `x`)"))
  expect_equal(translate_sql(quarter(x, with_year = TRUE)), sql("(DATENAME(YEAR, `x`) + '.' + DATENAME(QUARTER, `x`))"))
  expect_error(translate_sql(quarter(x, fiscal_start = 5)))
})

# verb translation --------------------------------------------------------

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

test_that("ORDER BY in subqueries uses TOP 9223372036854775807 (#337)", {
  local_options(dbplyr_table_num = 0)
  verify_output("sql/mssql-order-by.txt", {
    sql_select(simulate_mssql(), "x", "y", order_by = "z", bare_identifier_ok = TRUE)
  })
})

test_that("custom escapes translated correctly", {

  mf <- lazy_frame(x = "abc", con = simulate_mssql())

  a <- as_blob("abc")
  b <- as_blob(as.raw(c(0x01, 0x02)))

  expect_equal(
    mf %>% filter(x == a) %>% sql_render(),
    sql("SELECT *\nFROM `df`\nWHERE (`x` = 0x616263)")
  )

  L <- c(a, b)
  expect_equal(
    mf %>% filter(x %in% L) %>% sql_render(),
    sql("SELECT *\nFROM `df`\nWHERE (`x` IN (0x616263, 0x0102))")
  )

  expect_equal(
    mf %>% filter(x %in% !!L) %>% sql_render(),
    sql("SELECT *\nFROM `df`\nWHERE (`x` IN (0x616263, 0x0102))")
  )
})

# Live database -----------------------------------------------------------

test_that("mssql can copy_to() with temporary tables (#272)", {
  skip_if_no_db("mssql")

  df1 <- tibble(x = 1:3)

  expect_equal(
    src_test("mssql") %>%
      copy_to(df1, name = unique_table_name(), temporary = TRUE) %>%
      collect(),
    df1
  )
})

test_that("mssql can compute() with temporary tables (#272)", {
  skip_if_no_db("mssql")

  df1 <- tibble(x = 1:3)

  expect_equal(
    src_test("mssql") %>%
      copy_to(df1, name = unique_table_name(), temporary = TRUE) %>%
      mutate(x = x + 1L) %>%
      compute(temporary = TRUE) %>%
      collect(),
    df1 %>%
      mutate(x = x + 1L)
  )
})
