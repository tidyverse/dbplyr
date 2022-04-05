test_that("custom scalar & string functions translated correctly", {
  local_con(simulate_hive())

  expect_equal(translate_sql(bitwShiftL(x, 2L)),                sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(translate_sql(bitwShiftR(x, 2L)),                sql("SHIFTRIGHT(`x`, 2)"))
  expect_equal(translate_sql(cot(x)),                           sql("1.0 / TAN(`x`)"))
  expect_equal(translate_sql(str_replace_all(x, "old", "new")), sql("REGEXP_REPLACE(`x`, 'old', 'new')"))
  expect_equal(translate_sql(median(x, na.rm = TRUE)),          sql("PERCENTILE(`x`, 0.5) OVER ()"))
})

test_that("generates custom sql", {
  con <- simulate_hive()
  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))

  expect_snapshot(last_value_sql(con, ident("a")))

  lf <- lazy_frame(tibble(x = 1), con = con)
  expect_snapshot(union_all(lf, lf))
})
