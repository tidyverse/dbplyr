context("translate-Hive")

# Hive base_scalar conversions -----------------------------------------

test_that("cot() translates the appropiate formula ", {
  expect_equivalent(
    translate_sql(
      cot(field_name),
      con = simulate_hive()),
    sql("1 / TAN(`field_name`)"))
})
