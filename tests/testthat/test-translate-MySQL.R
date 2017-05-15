context("translate-MySQL")

test_that("use CHAR type for as.character", {
  context("translate-MSSQL")

  expect_equivalent(
    translate_sql(as.character(x), con = simulate_mysql()),
    sql("CAST(`x` AS CHAR)")
  )
})
