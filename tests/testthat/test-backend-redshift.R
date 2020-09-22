test_that("defaults to postgres translations", {
  local_con(simulate_redshift())
  expect_equal(translate_sql(log10(x)), sql("LOG(`x`)"))
})

test_that("string translations", {
  local_con(simulate_redshift())

  expect_error(translate_sql(str_replace("xx", ".", "a")), "not available")
  expect_equal(translate_sql(str_replace_all("xx", ".", "a")), sql("REGEXP_REPLACE('xx', '.', 'a')"))
})
