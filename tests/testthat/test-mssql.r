context("MSSQL translation")

# Initialize simulated DB connection -------------------------------------

mssql_connection <- dbplyr:::simulate_mssql()

# MSSQL base_scalar conversions -----------------------------------------

test_that("as.numeric field coercion translate to MSSQL NUMERIC ", {
  expect_equivalent(
    translate_sql(as.numeric(field_name), con = mssql_connection),
    sql("CAST(`field_name` AS NUMERIC)")
    )
})
test_that("as.double field coercion translate to MSSQL NUMERIC ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = mssql_connection),
    sql("CAST(`field_name` AS NUMERIC)"))
})
test_that("as.integer field coercion translate to MSSQL INT ", {
  expect_equivalent(
    translate_sql(as.integer(field_name), con = mssql_connection),
    sql("CAST(`field_name` AS INT)"))
})
test_that("as.logical field coercion translate to MSSQL BOOLEAN ", {
  expect_equivalent(
    translate_sql(as.logical(field_name), con = mssql_connection),
    sql("CAST(`field_name` AS BOOLEAN)"))
})
test_that("as.character field coercion translate to MSSQL VARCHAR(MAX) ", {
  expect_equivalent(
    translate_sql(as.character(field_name), con = mssql_connection),
    sql("CAST(`field_name` AS VARCHAR(MAX))"))
})
test_that("as.Date field coercion translate to MSSQL DATE ", {
  expect_equivalent(
    translate_sql(as.Date(field_name), con = mssql_connection),
    sql("CAST(`field_name` AS DATE)"))
})

test_that("| operator translate to MSSQL OR ", {
  expect_equivalent(
    translate_sql(field_name > 0 | field_name < 10, con = mssql_connection),
    sql("`field_name` > 0.0 OR `field_name` < 10.0"))
})
test_that("|| operator translate to MSSQL OR ", {
  expect_equivalent(
    translate_sql(field_name > 0 || field_name < 10, con = mssql_connection),
    sql("`field_name` > 0.0 OR `field_name` < 10.0"))
})
test_that("& operator translate to MSSQL AND ", {
  expect_equivalent(
    translate_sql(field_name > 0 & field_name < 10, con = mssql_connection),
    sql("`field_name` > 0.0 AND `field_name` < 10.0"))
})
test_that("&& operator translate to MSSQL AND ", {
  expect_equivalent(
    translate_sql(field_name > 0 && field_name < 10, con = mssql_connection),
    sql("`field_name` > 0.0 AND `field_name` < 10.0"))
})

test_that("paste function translate to MSSQL CONCAT ", {
  expect_equivalent(
    translate_sql(paste0(field1, field2), con = mssql_connection),
    sql("CONCAT(`field1`, `field2`)"))
})


# MSSQL base_agg conversions -----------------------------------------

test_that("count aggregate functions translate to MSSQL COUNT(*) ", {
  expect_equivalent(
    translate_sql(count(),
                  window = FALSE,
                  con = mssql_connection),
    sql("COUNT(*)"))
})

test_that("n aggregate functions translate to MSSQL COUNT(*) ", {
  expect_equivalent(
    translate_sql(n(),
                  window = FALSE,
                  con = mssql_connection),
    sql("COUNT(*)"))
})

test_that("n_distinct aggregate functions translate to MSSQL COUNT(DISTINCT) ", {
  expect_equivalent(
    translate_sql(n_distinct(field_name),
                  window = FALSE,
                  con = mssql_connection),
    sql("COUNT(DISTINCT(`field_name`))"))
})

test_that("sd aggregate functions translate to MSSQL STDEV ", {
  expect_equivalent(
    translate_sql(sd(field_name),
                  window = FALSE,
                  con = mssql_connection),
    sql("STDEV((`field_name`))"))
})

test_that("var aggregate functions translate to MSSQL VAR ", {
  expect_equivalent(
    translate_sql(var(field_name),
                  window = FALSE,
                  con = mssql_connection),
    sql("VAR((`field_name`))"))
})

# MSSQL query tests  ------------------------------------------------

test_that("query uses TOP instead of LIMIT ", {

  df <- data.frame(x = 1, y = 2)
  df_mssql <- tbl_lazy(df, src = simulate_mssql())

  expect_equivalent(
    show_query(head(df_mssql)),
    sql("SELECT  TOP 6 *\nFROM `df`"))
})




