test_that("tbl_sql() works with string argument", {
  withr::local_options(lifecycle_verbosity = "quiet")

  db <- local_memdb_frame("db", a = 1)
  expect_equal(collect(tbl_sql("sqlite", src_dbi(memdb()), "db")), collect(db))
})

test_that("tbl_sql() respects subclass argument", {
  withr::local_options(lifecycle_verbosity = "quiet")

  x <- tbl_sql("foo", src_dbi(memdb()), "abc", vars = letters)
  expect_s3_class(x, "tbl_foo")
})

test_that("same_src distinguishes srcs", {
  con1 <- local_sqlite_connection()
  con2 <- local_sqlite_connection()

  db1 <- copy_to(con1, iris[1:3, ], 'data1', temporary = FALSE)
  db2 <- copy_to(con2, iris[1:3, ], 'data2', temporary = FALSE)
  expect_true(same_src(db1, db1))
  expect_false(same_src(db1, db2))

  expect_false(same_src(db1, mtcars))
})

test_that("has nice print method", {
  mf <- copy_to_test("sqlite", tibble(x = 1, y = 1), name = "tbl_sum_test")
  expect_snapshot(mf, transform = scrub_sqlite_version)

  out2 <- mf |> group_by(x, y) |> arrange(x) |> mutate(z = x + y)
  expect_snapshot(out2, transform = scrub_sqlite_version)
})


# tbl ---------------------------------------------------------------------

test_that("can generate sql tbls with raw sql", {
  mf1 <- local_memdb_frame(x = 1:3, y = 3:1)
  mf2 <- tbl(mf1$src, sql(glue("SELECT * FROM {remote_name(mf1)}")))

  expect_equal(collect(mf1), collect(mf2))
})

test_that("can refer to default schema explicitly", {
  con <- local_sqlite_con_with_aux()
  DBI::dbExecute(con, "CREATE TABLE t1 (x)")

  expect_equal(as.character(tbl_vars(tbl(con, "t1"))), "x")
  expect_equal(as.character(tbl_vars(tbl(con, in_schema("main", "t1")))), "x")
})


test_that("can distinguish 'schema.table' from 'schema'.'table'", {
  con <- local_sqlite_con_with_aux()
  DBI::dbExecute(con, "CREATE TABLE aux.t1 (x, y, z)")
  DBI::dbExecute(con, "CREATE TABLE 'aux.t1' (a, b, c)")

  expect_equal(
    as.character(tbl_vars(tbl(con, in_schema("aux", "t1")))),
    c("x", "y", "z")
  )
  df <- tbl(con, ident("aux.t1"))
  expect_equal(as.character(tbl_vars(df)), c("a", "b", "c"))
})

test_that("useful error if missing I()", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(
    tbl(src_memdb(), "foo.bar"),
    error = TRUE
  )
})

test_that("check_from is deprecated", {
  con <- local_sqlite_connection()
  DBI::dbExecute(con, "CREATE TABLE x (y)")

  expect_snapshot(out <- tbl_sql("foo", src_dbi(con), "x", check_from = FALSE))
})

# n_groups ----------------------------------------------------------------

test_that("check basic group size implementation", {
  db <- local_memdb_frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
  expect_equal(n_groups(db), 1L)
  expect_equal(group_size(db), 30)

  gb <- group_by(db, x)
  expect_equal(n_groups(gb), 3L)
  expect_equal(group_size(gb), rep(10, 3))
})
