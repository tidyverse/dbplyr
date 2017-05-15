context("tbl_sql")

test_that("can generate sql tbls with raw sql", {
  mf1 <- memdb_frame(x = 1:3, y = 3:1)
  mf2 <- tbl(mf1$src, build_sql("SELECT * FROM ", mf1$ops$x))

  expect_equal(collect(mf1), collect(mf2))
})

test_that("tbl_sql() works with string argument", {
  name <- unclass(random_table_name())
  df <- memdb_frame(a = 1, .name = name)

  expect_equal(collect(tbl_sql("sqlite", df$src, name)), collect(df))
})

test_that("memdb_frame() returns visible output", {
  expect_true(withVisible(memdb_frame(a = 1))$visible)
})

test_that("head/print respects n" ,{
  df2 <- memdb_frame(x = 1:5)

  out <- df2 %>% head(n = Inf) %>% collect()
  expect_equal(nrow(out), 5)
  expect_output(print(df2, n = Inf))

  out <- df2 %>% head(n = 1) %>% collect()
  expect_equal(nrow(out), 1)

  expect_error(
    df2 %>% head(n = -1) %>% collect(),
    "not greater than 0"
  )
})

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
