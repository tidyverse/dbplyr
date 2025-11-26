test_that("tbl and src classes include connection class", {
  mf <- memdb_frame(x = 1, y = 2)
  expect_true(inherits(mf, "tbl_SQLiteConnection"))
  expect_true(inherits(mf$src, "src_SQLiteConnection"))
})

test_that("generates S3 class based on S4 class name", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_equal(
    connection_s3_class(con),
    c("src_SQLiteConnection", "src_dbi", "src_sql", "src")
  )

  on.exit(removeClass("Foo2"))
  on.exit(removeClass("Foo1"))

  Foo1 <- setClass("Foo1", contains = "DBIConnection")
  Foo2 <- setClass("Foo2", contains = "Foo1")
  expect_equal(
    connection_s3_class(Foo2()),
    c("src_Foo2", "src_Foo1", "src_dbi", "src_sql", "src")
  )
})
