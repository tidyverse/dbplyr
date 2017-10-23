context("translate-postgresql")

# odbc base_scalar conversions -----------------------------------------
test_that("round() coreces first arg to numeric and second to integer", {
  expect_equivalent(
    translate_sql(round(x, digits = 1.1), con = simulate_odbc_postgresql()),
    sql("ROUND((`x`)::numeric, 1)")
  )
})

test_that("cosh() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      cosh(field1),
      con = simulate_odbc_postgresql()),
    sql("(EXP(`field1`) + EXP(-`field1`)) / 2"))
})
test_that("sinh() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      sinh(field1),
      con = simulate_odbc_postgresql()),
    sql("(EXP(`field1`) - EXP(-`field1`)) / 2"))
})
test_that("tanh() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      tanh(field1),
      con = simulate_odbc_postgresql()),
    sql("((EXP(`field1`) - EXP(-`field1`)) / 2) / ((EXP(`field1`) + EXP(-`field1`)) / 2)"))
})
test_that("coth() translates to the correct formula ", {
  expect_equivalent(
    translate_sql(
      coth(field1),
      con = simulate_odbc_postgresql()),
    sql("((EXP(`field1`) + EXP(-`field1`)) / 2) / ((EXP(`field1`) - EXP(-`field1`)) / 2)"))
})
