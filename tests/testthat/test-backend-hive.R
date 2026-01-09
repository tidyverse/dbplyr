test_that("custom scalar & string functions translated correctly", {
  con <- simulate_hive()

  expect_translation(con, bitwShiftL(x, 2L), 'SHIFTLEFT("x", 2)')
  expect_translation(con, bitwShiftR(x, 2L), 'SHIFTRIGHT("x", 2)')
  expect_translation(con, cot(x), '1.0 / TAN("x")')
  expect_translation(
    con,
    str_replace_all(x, "old", "new"),
    'REGEXP_REPLACE("x", \'old\', \'new\')'
  )
  expect_translation(
    con,
    median(x, na.rm = TRUE),
    'PERCENTILE("x", 0.5) OVER ()'
  )
})

test_that("generates custom sql", {
  con <- simulate_hive()
  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))

  expect_equal(
    translate_sql(last(x, na_rm = TRUE), vars_order = "a", con = con),
    sql(
      'LAST_VALUE("x", TRUE) OVER (ORDER BY "a" ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)'
    )
  )

  lf <- lazy_frame(tibble(x = 1), con = con)
  expect_snapshot(union_all(lf, lf))
})
