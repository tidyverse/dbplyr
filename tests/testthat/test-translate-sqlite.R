context("translate-sqlite")

test_that("na_if is translated to NULLIF", {
  expect_equivalent(translate_sql(na_if(x, 0L), con = simulate_sqlite()), sql('NULLIF(`x`, 0)'))
})
