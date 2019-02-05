context("test-dbi-s3")


test_that("db_write_table calls dbQuoteIdentifier on table name" ,{
  idents <- character()

  setClass("DummyDBIConnection", representation("DBIConnection"))
  setMethod("dbQuoteIdentifier", c("DummyDBIConnection", "character"),
    function(conn, x, ...) {
      idents <<- c(idents, x)
    }
  )

  setMethod("dbWriteTable", c("DummyDBIConnection", "character", "ANY"),
    function(conn, name, value, ...) {TRUE}
  )

  dummy_con <- new("DummyDBIConnection")
  db_write_table(dummy_con, "somecrazytablename", NA, NA)
  expect_true("somecrazytablename" %in% idents)
})
