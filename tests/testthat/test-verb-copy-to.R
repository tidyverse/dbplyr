test_that("can copy to from remote sources", {
  df <- tibble(x = 1:10)
  con1 <- local_sqlite_connection()
  df_1 <- copy_to(con1, df, "df1")

  # Create from tbl in same database
  df_2 <- copy_to(con1, df_1, "df2")
  expect_equal(collect(df_2), df)

  # Create from tbl in another data
  con2 <- local_sqlite_connection()
  df_3 <- copy_to(con2, df_1, "df3")
  expect_equal(collect(df_3), df)
})

test_that("only overwrite existing table if explicitly requested", {
  con <- local_sqlite_connection()
  local_db_table(con, data.frame(x = 1:5), "df")

  expect_error(copy_to(con, data.frame(x = 1), name = "df"), "exists")
  expect_silent(copy_to(con, data.frame(x = 1), name = "df", overwrite = TRUE))
})

test_that("overwrite flag works when copying *within* db sources", {
  con <- local_sqlite_connection()
  df <- copy_to(con, tibble(x = 1), "df", temporary = FALSE)
  copy_to(con, df, "df2", temporary = FALSE)

  df2 <- tibble(x = 2)
  out <- copy_to(con, df2, name = "df2", temporary = FALSE, overwrite = TRUE)
  expect_equal(collect(out), df2)
})

test_that("can create a new table in non-default schema", {
  con <- local_sqlite_con_with_aux()

  df1 <- tibble(x = 1)
  df2 <- tibble(x = 2)

  db1 <- copy_to(con, df1, in_schema("aux", "df"), temporary = FALSE)
  expect_equal(collect(db1), df1)

  # And can overwrite
  db2 <- copy_to(
    con,
    df2,
    in_schema("aux", "df"),
    temporary = FALSE,
    overwrite = TRUE
  )
  expect_equal(collect(db2), df2)
})

test_that("df must be a local or remote table", {
  con <- local_sqlite_connection()

  expect_snapshot(error = TRUE, copy_to(con, list(x = 1), name = "df"))
})

# as_copy() ---------------------------------------------------------------

test_that("as_copy() converts TRUE/FALSE to enum", {
  expect_equal(as_copy(TRUE), "temp-table")
  expect_equal(as_copy(FALSE), "none")
})

test_that("as_copy() passes through valid strings", {
  expect_equal(as_copy("none"), "none")
  expect_equal(as_copy("temp-table"), "temp-table")
  expect_equal(as_copy("inline"), "inline")
})

test_that("as_copy() errors on invalid values", {
  expect_snapshot(as_copy("other"), error = TRUE)
})

# dbplyr_auto_copy() ----------------------------------------------------

test_that("dbplyr_auto_copy() returns y if same source", {
  df <- local_memdb_frame(x = 1:3)
  expect_identical(dbplyr_auto_copy(df, df, copy = FALSE), df)
})

test_that("dbplyr_auto_copy() errors when copy = FALSE and different sources", {
  df <- local_memdb_frame(x = 1:3)
  local_df <- tibble(x = 1:3)

  expect_snapshot(dbplyr_auto_copy(df, local_df, copy = FALSE), error = TRUE)
  expect_snapshot(dbplyr_auto_copy(df, local_df, copy = "none"), error = TRUE)
})

test_that("dbplyr_auto_copy() with copy = TRUE copies to temp table", {
  df <- local_memdb_frame(x = 1:3)
  local_df <- tibble(x = 1:3)

  out <- dbplyr_auto_copy(df, local_df, copy = TRUE)
  expect_s3_class(out, "tbl_sql")
  expect_equal(collect(out), local_df)
})

test_that("dbplyr_auto_copy() with copy = 'inline' uses copy_inline", {
  df <- local_memdb_frame(x = 1:3)
  local_df <- tibble(x = 1:3)

  out <- dbplyr_auto_copy(df, local_df, copy = "inline")
  expect_s3_class(out, "tbl_sql")
  expect_s3_class(out$lazy_query, "lazy_base_values_query")
  expect_equal(collect(out), local_df)
})
