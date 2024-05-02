test_that("tbl_sql() works with string argument", {
  name <- unclass(unique_table_name())
  df <- memdb_frame(a = 1, .name = name)

  expect_equal(collect(tbl_sql("sqlite", df$src, name)), collect(df))
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

# tbl ---------------------------------------------------------------------

test_that("can generate sql tbls with raw sql", {
  mf1 <- memdb_frame(x = 1:3, y = 3:1)
  mf2 <- tbl(mf1$src, sql(glue("SELECT * FROM {remote_name(mf1)}")))

  expect_equal(collect(mf1), collect(mf2))
})

test_that("sql tbl can be printed", {
  mf1 <- memdb_frame(x = 1:3, y = 3:1)
  mf2 <- tbl(mf1$src, sql(glue("SELECT * FROM {remote_name(mf1)}")))

  expect_snapshot(mf2, transform = function(x) {
    gsub("sqlite .* \\[:memory:\\]", "sqlite ?.?.? [:memory:]", x)
  })
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

  expect_equal(as.character(tbl_vars(tbl(con, in_schema("aux", "t1")))), c("x", "y", "z"))
  df <- tbl(con, ident("aux.t1"))
  expect_equal(as.character(tbl_vars(df)), c("a", "b", "c"))
})

test_that("useful error if missing I()", {
  expect_snapshot(
    tbl(src_memdb(), "foo.bar"),
    error = TRUE
  )
})

test_that("check_from is deprecated", {
  con <- local_sqlite_connection()
  DBI::dbExecute(con, "CREATE TABLE x (y)")

  expect_snapshot(out <- tbl(con, "x", check_from = FALSE))
})

# n_groups ----------------------------------------------------------------

test_that("check basic group size implementation", {
  db <- memdb_frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
  expect_equal(n_groups(db), 1L)
  expect_equal(group_size(db), 30)

  gb <- group_by(db, x)
  expect_equal(n_groups(gb), 3L)
  expect_equal(group_size(gb), rep(10, 3))
})

# tbl_sum -------------------------------------------------------------------

test_that("ungrouped output", {
  mf <- copy_to_test("sqlite", tibble(x = 1:5, y = 1:5), name = "tbl_sum_test")

  out1 <- tbl_sum(mf)
  expect_named(out1, c("Source", "Database"))
  expect_equal(out1[["Source"]], "table<`tbl_sum_test`> [?? x 2]")
  expect_match(out1[["Database"]], "sqlite (.*) \\[:memory:\\]")

  out2 <- tbl_sum(mf %>% group_by(x, y))
  expect_named(out2, c("Source", "Database", "Groups"))
  expect_equal(out2[["Groups"]], c("x, y"))

  out3 <- tbl_sum(mf %>% arrange(x))
  expect_named(out3, c("Source", "Database", "Ordered by"))
  expect_equal(out3[["Ordered by"]], c("x"))
})
