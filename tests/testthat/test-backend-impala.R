test_that("custom scalar functions translated correctly", {
  local_con(simulate_impala())

  expect_equal(test_translate_sql(as.Date(x)), sql("CAST(`x` AS VARCHAR(10))"))
  expect_equal(test_translate_sql(ceiling(x)), sql("CEIL(`x`)"))
})

test_that("custom bitwise operations translated correctly", {
  local_con(simulate_impala())

  expect_equal(test_translate_sql(bitwNot(x)), sql("BITNOT(`x`)"))
  expect_equal(test_translate_sql(bitwAnd(x, 128L)), sql("BITAND(`x`, 128)"))
  expect_equal(test_translate_sql(bitwOr(x, 128L)), sql("BITOR(`x`, 128)"))
  expect_equal(test_translate_sql(bitwXor(x, 128L)), sql("BITXOR(`x`, 128)"))
  expect_equal(test_translate_sql(bitwShiftL(x, 2L)), sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(test_translate_sql(bitwShiftR(x, 2L)), sql("SHIFTRIGHT(`x`, 2)"))
})

test_that("generates custom sql", {
  con <- simulate_impala()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
})
