context("translate-MSSQL")

# MSSQL base_scalar conversions -----------------------------------------

test_that("as.numeric() translated to NUMERIC ", {
  expect_equivalent(
    translate_sql(as.numeric(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS NUMERIC)")
  )
})
test_that("as.double() translated to NUMERIC ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS NUMERIC)")
  )
})
test_that("as.integer() translated to INT ", {
  expect_equivalent(
    translate_sql(as.integer(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS INT)")
  )
})
test_that("as.logical() translate to BOOLEAN ", {
  expect_equivalent(
    translate_sql(as.logical(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS BOOLEAN)")
  )
})
test_that("as.character() translate to VARCHAR(MAX) ", {
  expect_equivalent(
    translate_sql(as.character(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS VARCHAR(MAX))")
  )
})
test_that("as.Date() translate to DATE ", {
  expect_equivalent(
    translate_sql(as.Date(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS DATE)")
  )
})

test_that("paste function translate to CONCAT ", {
  expect_equivalent(
    translate_sql(paste0(field1, field2), con = simulate_mssql()),
    sql("CONCAT(`field1`, `field2`)")
  )
})

# MSSQL base_agg conversions -----------------------------------------

test_that("n() translated to COUNT(*) ", {
  expect_equivalent(
    translate_sql(n(), window = FALSE, con = simulate_mssql()),
    sql("COUNT(*)")
  )
})

test_that("n_distinct() translated to COUNT(DISTINCT) ", {
  expect_equivalent(
    translate_sql(n_distinct(field_name), window = FALSE, con = simulate_mssql()),
    sql("COUNT(DISTINCT(`field_name`))")
  )
})

test_that("sd aggregate functions translate to STDEV ", {
  expect_equivalent(
    translate_sql(sd(field_name), window = FALSE, con = simulate_mssql()),
    sql("STDEV((`field_name`))")
  )
})

test_that("var aggregate functions translate to VAR ", {
  expect_equivalent(
    translate_sql(var(field_name), window = FALSE, con = simulate_mssql()),
    sql("VAR((`field_name`))")
  )
})

# MSSQL query tests  ------------------------------------------------

test_that("query uses TOP instead of LIMIT ", {
  df <- data.frame(x = 1, y = 2)
  df_mssql <- tbl_lazy(df, src = simulate_mssql())

  expect_equivalent(
    show_query(head(df_mssql)),
    sql("SELECT  TOP 6 *\nFROM `df`"))
})
