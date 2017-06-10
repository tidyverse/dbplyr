context("translate-Oracle")

# Oracle base_scalar conversions -----------------------------------------

test_that("as.numeric() translated to VARCHAR(255) ", {
  expect_equivalent(
    translate_sql(as.character(field_name), con = simulate_oracle()),
    sql("CAST(`field_name` AS VARCHAR(255))")
  )
})

test_that("as.numeric() translated to NUMBER ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = simulate_oracle()),
    sql("CAST(`field_name` AS NUMBER)")
  )
})

# Oracle query tests  ------------------------------------------------

test_that("query uses FETCH FIRST instead of LIMIT ", {
  df <- data.frame(x = 1, y = 2)
  df_oracle <- tbl_lazy(df, src = simulate_oracle())

  expect_equivalent(
    dplyr::show_query(head(df_oracle)),
    sql("SELECT *\nFROM (df) \nFETCH FIRST 6 ROWS ONLY "))
})

