test_that("custom scalar translated correctly", {
  local_con(simulate_teradata())

  expect_equal(translate_sql(x != y),          sql("`x` <> `y`"))
  expect_equal(translate_sql(as.numeric(x)),   sql("CAST(`x` AS NUMERIC)"))
  expect_equal(translate_sql(as.double(x)),    sql("CAST(`x` AS NUMERIC)"))
  expect_equal(translate_sql(as.character(x)), sql("CAST(`x` AS VARCHAR(MAX))"))
  expect_equal(translate_sql(log(x)),          sql("LN(`x`)"))
  expect_equal(translate_sql(cot(x)),          sql("1 / TAN(`x`)"))
  expect_equal(translate_sql(nchar(x)),        sql("CHARACTER_LENGTH(`x`)"))
  expect_equal(translate_sql(ceil(x)),         sql("CEILING(`x`)"))
  expect_equal(translate_sql(ceiling(x)),      sql("CEILING(`x`)"))
  expect_equal(translate_sql(atan2(x, y)),     sql("ATAN2(`y`, `x`)"))
  expect_equal(translate_sql(substr(x, 1, 2)), sql("SUBSTR(`x`, 1.0, 2.0)"))

  expect_error(translate_sql(paste(x)),        sql("not supported"))
})

test_that("custom bitwise operations translated correctly", {
  local_con(simulate_teradata())

  expect_equal(translate_sql(bitwNot(x)),        sql("BITNOT(`x`)"))
  expect_equal(translate_sql(bitwAnd(x, 128L)),  sql("BITAND(`x`, 128)"))
  expect_equal(translate_sql(bitwOr(x, 128L)),   sql("BITOR(`x`, 128)"))
  expect_equal(translate_sql(bitwXor(x, 128L)),  sql("BITXOR(`x`, 128)"))
  expect_equal(translate_sql(bitwShiftL(x, 2L)), sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(translate_sql(bitwShiftR(x, 2L)), sql("SHIFTRIGHT(`x`, 2)"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_teradata())

  expect_equal(translate_sql(var(x, na.rm = TRUE), window = FALSE), sql("VAR_SAMP(`x`)"))
})

test_that("custom window functions translated correctly", {
  local_con(simulate_teradata())

  expect_equal(translate_sql(var(x, na.rm = TRUE)), sql("VAR_SAMP(`x`) OVER ()"))
})

test_that("generates custom sql", {
  con <- simulate_teradata()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
})

# verb translation --------------------------------------------------------

test_that("head translated to TOP", {
  mf <- lazy_frame(x = 1, con = simulate_teradata())
  expect_snapshot(mf %>% head() %>% sql_render())
})
