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

test_that("can round trip basic data frame", {
  df <- test_frame(x = c(1, 10, 9, NA), y = letters[1:4])
  expect_equal_tbls(df)
})

test_that("NAs in character fields handled by db sources (#2256)", {
  df <- test_frame(
    x = c("a", "aa", NA),
    y = c(NA, "b", "bb"),
    z = c("cc", NA, "c")
  )
  expect_equal_tbls(df)
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
