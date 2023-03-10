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

test_that("can create a new table in non-default schema", {
  con <- local_sqlite_con_with_aux()

  df1 <- tibble(x = 1)
  df2 <- tibble(x = 2)

  db1 <- copy_to(con, df1, in_schema("aux", "df"), temporary = FALSE)
  expect_equal(collect(db1), df1)

  # And can overwrite
  db2 <- copy_to(con, df2, in_schema("aux", "df"), temporary = FALSE, overwrite = TRUE)
  expect_equal(collect(db2), df2)
})

test_that("df must be a local or remote table", {
  con <- local_sqlite_connection()

  expect_snapshot(error = TRUE, copy_to(con, list(x = 1), name = "df"))
})

# copy_inline() -----------------------------------------------------------

test_that("can translate a table", {
  con <- local_sqlite_connection()
  df <- tibble(
    lgl = TRUE,
    int = 1L,
    dbl = 1.5,
    chr = "a",
    date = as.Date("2020-01-01", tz = "UTC"),
    dtt = as.POSIXct("2020-01-01 01:23:45", tz = "UTC")
  )

  expect_snapshot(copy_inline(con, df) %>% remote_query())

  expect_equal(
    copy_inline(con, df) %>% collect(),
    tibble(
      lgl = 1L,
      int = 1L,
      dbl = 1.5,
      chr = "a",
      date = "2020-01-01",
      dtt = "2020-01-01T01:23:45Z"
    )
  )

  expect_equal(
    copy_inline(con, tibble(date = as.Date(c("2020-01-01", "2020-01-02"), tz = "UTC"))) %>%
      collect(),
    tibble(date = c("2020-01-01", "2020-01-02"))
  )
})

test_that("can translate 1-column tables", {
  con <- local_sqlite_connection()
  expect_snapshot(
    copy_inline(con, tibble(dbl = 1.5)) %>%
      remote_query()
  )
})

test_that("zero row table works", {
  con <- local_sqlite_connection()
  expect_snapshot(
    copy_inline(con, tibble(dbl = numeric(), chr = character())) %>%
      remote_query()
  )

  expect_snapshot(
    copy_inline(con, tibble(dbl = numeric())) %>%
      remote_query()
  )
})

test_that("types argument works", {
  con <- local_sqlite_connection()

  df <- tibble(x = "1", y = 2L)
  expect_equal(copy_inline(con, df) %>% collect(), df)

  expect_equal(
    copy_inline(con, df, types = c(x = "INTEGER", y = "TEXT")) %>% collect(),
    tibble(x = 1L, y = "2")
  )
})

test_that("checks inputs", {
  con <- simulate_dbi()

  expect_snapshot({
    (expect_error(copy_inline(con, tibble())))
    (expect_error(copy_inline(con, lazy_frame(a = 1))))

    (expect_error(copy_inline(con, tibble(a = 1), types = c(b = "bigint"))))
    (expect_error(copy_inline(con, tibble(a = 1), types = c(b = 1))))
  })
})
