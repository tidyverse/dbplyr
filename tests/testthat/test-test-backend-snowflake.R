test_that("custom scalar translated correctly", {
  local_con(simulate_snowflake())
  expect_equal(translate_sql(log10(x)), sql("LOG(10.0, `x`)"))
  expect_equal(translate_sql(round(x, digits = 1.1)), sql("ROUND((`x`) :: FLOAT, 1)"))
})
