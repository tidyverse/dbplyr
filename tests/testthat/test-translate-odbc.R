context("translate-odbc")

# odbc base_scalar conversions -----------------------------------------

test_that("as.numeric() translates to DOUBLE ", {
  expect_equivalent(
    translate_sql(
      as.numeric(field_name),
      con = simulate_odbc("OdbcConnection")),
    sql("CAST(`field_name` AS DOUBLE)")
    )
})
test_that("as.double() translates to DOUBLE ", {
  expect_equivalent(
    translate_sql(
      as.double(field_name),
      con = simulate_odbc("OdbcConnection")),
    sql("CAST(`field_name` AS DOUBLE)"))
})
test_that("as.integer() translates INT ", {
  expect_equivalent(
    translate_sql(
      as.integer(field_name),
      con = simulate_odbc("OdbcConnection")),
    sql("CAST(`field_name` AS INT)"))
})
test_that("as.logical() translates to BOOLEAN ", {
  expect_equivalent(
    translate_sql(
      as.logical(field_name),
      con = simulate_odbc("OdbcConnection")),
    sql("CAST(`field_name` AS BOOLEAN)"))
})
test_that("as.character() translates to STRING ", {
  expect_equivalent(
    translate_sql(
      as.character(field_name),
      con = simulate_odbc("OdbcConnection")),
    sql("CAST(`field_name` AS STRING)"))
})
test_that("as.Date() translates to DATE ", {
  expect_equivalent(
    translate_sql(
      as.Date(field_name),
      con = simulate_odbc("OdbcConnection")),
    sql("CAST(`field_name` AS DATE)"))
})
test_that("round() translates to ROUND ", {
  expect_equivalent(
    translate_sql(
      round(10.1),
      con = simulate_odbc("OdbcConnection")),
    sql("ROUND(10.1, 0)"))
})

test_that("the digits argument in round() is passed to ROUND ", {
  expect_equivalent(
    translate_sql(
      round(10.1, digits = 1),
      con = simulate_odbc("OdbcConnection")),
    sql("ROUND(10.1, 1)"))
})
test_that("paste0() translates to CONCAT ", {
  expect_equivalent(
    translate_sql(
      paste0(field1, field2),
      con = simulate_odbc("OdbcConnection")),
    sql("CONCAT(`field1`, `field2`)"))
})

test_that("paste() translates to CONCAT_WS ", {
  expect_equivalent(
    translate_sql(
      paste(field1, field2),
      con = simulate_odbc("OdbcConnection")),
    sql("CONCAT_WS(' ', `field1`,`field2`)"))
})
test_that("the sep argument in paste() is passed to CONCAT_WS ", {
  expect_equivalent(
    translate_sql(
      paste(field1, field2, sep = ","),
      con = simulate_odbc("OdbcConnection")),
    sql("CONCAT_WS(',', `field1`,`field2`)"))
})

# odbc base_agg conversions -----------------------------------------

test_that("count() translates to COUNT(*) ", {
  expect_equivalent(
    translate_sql(count(),
                  window = FALSE,
                  con = simulate_odbc("OdbcConnection")),
    sql("COUNT(*)"))
})

test_that("n() translates to COUNT(*) ", {
  expect_equivalent(
    translate_sql(n(),
                  window = FALSE,
                  con = simulate_odbc("OdbcConnection")),
    sql("COUNT(*)"))
})

test_that("n_distinct() translates to COUNT(DISTINCT) ", {
  expect_equivalent(
    translate_sql(n_distinct(field_name),
                  window = FALSE,
                  con = simulate_odbc("OdbcConnection")),
    sql("COUNT(DISTINCT `field_name`)"))
})

test_that("sd() translates to STDEV ", {
  expect_equivalent(
    translate_sql(sd(field_name),
                  window = FALSE,
                  con = simulate_odbc("OdbcConnection")),
    sql("SD(`field_name`)"))
})

test_that("var() translates to VARIANCE ", {
  expect_equivalent(
    translate_sql(var(field_name),
                  window = FALSE,
                  con = simulate_odbc("OdbcConnection")),
    sql("VARIANCE(`field_name`)"))
})

# odbc query tests  ------------------------------------------------

df <- data.frame(x = 1, y = 2)
df_odbc <- tbl_lazy(df, src = simulate_odbc("OdbcConnection"))

test_that("query uses COUNT(*) instead of COUNT() ", {
  expect_equivalent(
    show_query(tally(df_odbc)),
    sql("SELECT COUNT(*) AS `n`\nFROM `df`"))
})

test_that("query uses COUNT(*) instead of COUNT() ", {
  expect_equivalent(
    show_query(summarise(df_odbc, count = n())),
    sql("SELECT COUNT(*) AS `count`\nFROM `df`"))
})





