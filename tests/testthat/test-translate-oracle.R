context("translate-Oracle")

# Oracle base_scalar conversions -----------------------------------------

test_that("as.numeric() translated to VARCHAR(255) ", {
  expect_equivalent(
    translate_sql(as.character(field_name), con = simulate_oracle()),
    sql("CAST(\"field_name\" AS VARCHAR(255))")
  )
})

test_that("as.numeric() translated to NUMBER ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = simulate_oracle()),
    sql("CAST(\"field_name\" AS NUMBER)")
  )
})
