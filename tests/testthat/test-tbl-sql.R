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

  out <- df2 %>% head(n = 0) %>% collect()
  expect_equal(nrow(out), 0)

  expect_error(
    df2 %>% head(n = -1) %>% collect(),
    "not greater than or equal to 0"
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

test_that("same_src distinguishes srcs", {
  src1 <- src_sqlite(":memory:", create = TRUE)
  src2 <- src_sqlite(":memory:", create = TRUE)
  expect_true(same_src(src1, src1))
  expect_false(same_src(src1, src2))

  db1 <- copy_to(src1, iris, 'data1', temporary = FALSE)
  db2 <- copy_to(src2, iris, 'data2', temporary = FALSE)
  expect_true(same_src(db1, db1))
  expect_false(same_src(db1, db2))

  expect_false(same_src(db1, mtcars))
})

test_that("can copy to from remote sources", {
  df <- data.frame(x = 1:10)
  con1 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con1))
  df_1 <- copy_to(con1, df, "df1")

  # Create from tbl in same database
  df_2 <- copy_to(con1, df_1, "df2")
  expect_equal(collect(df_2), df)

  # Create from tbl in another data
  con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con2))
  df_3 <- copy_to(con2, df_1, "df3")
  expect_equal(collect(df_3), df)
})
