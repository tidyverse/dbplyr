context("test-tbl_sql.R")

test_that("tbl_sql() works with string argument", {
  name <- unclass(unique_table_name())
  df <- memdb_frame(a = 1, .name = name)

  expect_equal(collect(tbl_sql("sqlite", df$src, name)), collect(df))
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

# tbl ---------------------------------------------------------------------

test_that("can generate sql tbls with raw sql", {
  mf1 <- memdb_frame(x = 1:3, y = 3:1)
  mf2 <- tbl(mf1$src, build_sql("SELECT * FROM ", mf1$ops$x, con = simulate_dbi()))

  expect_equal(collect(mf1), collect(mf2))
})

test_that("can refer to default schema explicitly", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t1 (x)")

  expect_equal(tbl_vars(tbl(con, "t1")), "x")
  expect_equal(tbl_vars(tbl(con, in_schema("main", "t1"))), "x")
})

test_that("can distinguish 'schema.table' from 'schema'.'table'", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE aux.t1 (x, y, z)")
  DBI::dbExecute(con, "CREATE TABLE 'aux.t1' (a, b, c)")

  expect_equal(tbl_vars(tbl(con, in_schema("aux", "t1"))), c("x", "y", "z"))
  expect_equal(tbl_vars(tbl(con, ident("aux.t1"))), c("a", "b", "c"))
})

# n_groups ----------------------------------------------------------------

# Data for the first three test_that groups below
df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
# MariaDB returns bit64 instead of int, which makes testing hard
tbls <- test_load(df, ignore = "MariaDB")

test_that("ungrouped data has 1 group, with group size = nrow()", {
  for (tbl in tbls) {
    expect_equal(n_groups(tbl), 1L)
    expect_equal(group_size(tbl), 30)
  }
})

test_that("rowwise data has one group for each group", {
  rw <- rowwise(df)
  expect_equal(n_groups(rw), 30)
  expect_equal(group_size(rw), rep(1, 30))
})

test_that("group_size correct for grouped data", {
  for (tbl in tbls) {
    grp <- group_by(tbl, x)
    expect_equal(n_groups(grp), 3L)
    expect_equal(group_size(grp), rep(10, 3))
  }
})

# tbl_sum -------------------------------------------------------------------

test_that("ungrouped output", {
  mf <- memdb_frame(x = 1:5, y = 1:5, .name = "tbl_sum_test")

  out1 <- tbl_sum(mf)
  expect_named(out1, c("Source", "Database"))
  expect_equal(out1[["Source"]], "table<tbl_sum_test> [?? x 2]")
  expect_match(out1[["Database"]], "sqlite (.*) \\[:memory:\\]")

  out2 <- tbl_sum(mf %>% group_by(x, y))
  expect_named(out2, c("Source", "Database", "Groups"))
  expect_equal(out2[["Groups"]], c("x, y"))

  out3 <- tbl_sum(mf %>% arrange(x))
  expect_named(out3, c("Source", "Database", "Ordered by"))
  expect_equal(out3[["Ordered by"]], c("x"))
})
