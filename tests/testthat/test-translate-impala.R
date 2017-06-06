context("translate-Impala")

# Impala base_scalar conversions -----------------------------------------

test_that("as.Date()translates to VARCHAR(10) ", {
  expect_equivalent(
    translate_sql(
      as.Date(field_name),
      con = simulate_impala()),
    sql("CAST(`field_name` AS VARCHAR(10))"))
})
test_that("ceiling() translates to CEIL ", {
  expect_equivalent(
    translate_sql(
      ceiling(field_name),
      con = simulate_impala()),
    sql("CEIL(`field_name`)"))
})
