context("MSSQL translation")

# DB simulator -----------------------------------------------------------

# Generic function to simulate any connection
# coming from 'odbc' by passing it the dbms.name

class_cache <- new.env(parent = emptyenv())

simulate_db <- function(dbms.name = NULL) {
  class <- getClassDef(dbms.name, where = class_cache, inherits = FALSE)
  if (is.null(class)) {
    setClass(dbms.name,
             contains = "OdbcConnection", where = class_cache)
  }
  new(dbms.name, quote = "\"")
}

# Initialize simulated DB connection -------------------------------------

mssql_connection <- simulate_db("Microsoft SQL Server")


# MSSQL base_scalar conversions -----------------------------------------

test_that("field coercion translate to MSSQL specifications ", {
  expect_equivalent(translate_sql(as.numeric(field_name), con = mssql_connection), sql("CAST(\"field_name\" AS NUMERIC)"))
  expect_equivalent(translate_sql(as.double(field_name), con = mssql_connection), sql("CAST(\"field_name\" AS NUMERIC)"))
  expect_equivalent(translate_sql(as.integer(field_name), con = mssql_connection), sql("CAST(\"field_name\" AS INT)"))
  expect_equivalent(translate_sql(as.logical(field_name), con = mssql_connection), sql("CAST(\"field_name\" AS BOOLEAN)"))
  expect_equivalent(translate_sql(as.character(field_name), con = mssql_connection), sql("CAST(\"field_name\" AS VARCHAR(MAX))"))
  expect_equivalent(translate_sql(as.Date(field_name), con = mssql_connection), sql("CAST(\"field_name\" AS DATE)"))

})

test_that("operators translate to MSSQL specifications ", {
  expect_equivalent(translate_sql(field_name > 0 | field_name < 10, con = mssql_connection), sql("\"field_name\" > 0.0 OR \"field_name\" < 10.0"))
  expect_equivalent(translate_sql(field_name > 0 || field_name < 10, con = mssql_connection), sql("\"field_name\" > 0.0 OR \"field_name\" < 10.0"))
  expect_equivalent(translate_sql(field_name > 0 & field_name < 10, con = mssql_connection), sql("\"field_name\" > 0.0 AND \"field_name\" < 10.0"))
  expect_equivalent(translate_sql(field_name > 0 && field_name < 10, con = mssql_connection), sql("\"field_name\" > 0.0 AND \"field_name\" < 10.0"))
})


test_that("other functions translate to MSSQL specifications ", {
  expect_equivalent(translate_sql(paste0(field1, field2), con = mssql_connection), sql("CONCAT(\"field1\", \"field2\")"))
})


# MSSQL base_agg conversions -----------------------------------------

test_that("aggregate functions translate to MSSQL specifications ", {
  expect_equivalent(translate_sql(count(), window = FALSE,con = mssql_connection), sql("COUNT(*)"))
  expect_equivalent(translate_sql(n(), window = FALSE,con = mssql_connection), sql("COUNT(*)"))
  expect_equivalent(translate_sql(n_distinct(field_name), window = FALSE,con = mssql_connection), sql("COUNT(DISTINCT(\"field_name\"))"))
  expect_equivalent(translate_sql(sd(field_name), window = FALSE,con = mssql_connection), sql("STDEV((\"field_name\"))"))
  expect_equivalent(translate_sql(var(field_name), window = FALSE,con = mssql_connection), sql("VAR((\"field_name\"))"))

})





