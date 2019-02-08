context("test-copy-to")


test_that("can copy to from remote sources", {
  df <- data.frame(x = 1:10)
  con1 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con1), add = TRUE)
  df_1 <- copy_to(con1, df, "df1")

  # Create from tbl in same database
  df_2 <- copy_to(con1, df_1, "df2")
  expect_equal(collect(df_2), df)

  # Create from tbl in another data
  con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
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
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "df", data.frame(x = 1:5))

  expect_error(copy_to(con, data.frame(x = 1), name = "df"), "exists")
  expect_silent(copy_to(con, data.frame(x = 1), name = "df", overwrite = TRUE))
})

test_that("can create a new table in non-default schema", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  aux_mtcars <- copy_to(con, mtcars, in_schema("aux", "mtcars"), temporary = FALSE)

  expect_equal(tbl_vars(aux_mtcars), tbl_vars(mtcars))
})
