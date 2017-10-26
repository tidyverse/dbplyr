context("translate-Hive")

# Hive base_scalar conversions -----------------------------------------

test_that("cot() translates the appropiate formula ", {
  expect_equivalent(
    translate_sql(
      cot(field_name),
      con = simulate_hive()),
    sql("1.0 / TAN(`field_name`)"))
})

# stringr -------------------------------------------

test_that("str_replace_all() translates correctly ", {
  expect_equivalent(
    translate_sql(
      str_replace_all(field_name, "old", "new"),
      con = simulate_hive()),
      sql("REGEXP_REPLACE(`field_name`, 'old', 'new')"))
})
