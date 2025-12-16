test_that("custom scalar functions translated correctly", {
  con <- simulate_impala()

  expect_translation(con, as.Date(x), 'CAST("x" AS VARCHAR(10))')
  expect_translation(con, ceiling(x), 'CEIL("x")')
})

test_that("custom bitwise operations translated correctly", {
  con <- simulate_impala()

  expect_translation(con, bitwNot(x), 'BITNOT("x")')
  expect_translation(con, bitwAnd(x, 128L), 'BITAND("x", 128)')
  expect_translation(con, bitwOr(x, 128L), 'BITOR("x", 128)')
  expect_translation(con, bitwXor(x, 128L), 'BITXOR("x", 128)')
  expect_translation(con, bitwShiftL(x, 2L), 'SHIFTLEFT("x", 2)')
  expect_translation(con, bitwShiftR(x, 2L), 'SHIFTRIGHT("x", 2)')
})

test_that("generates custom sql", {
  con <- simulate_impala()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
})
