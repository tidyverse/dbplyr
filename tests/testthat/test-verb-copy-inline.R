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

  expect_snapshot(copy_inline(con, df) |> remote_query())

  expect_equal(
    copy_inline(con, df) |> collect(),
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
    copy_inline(
      con,
      tibble(date = as.Date(c("2020-01-01", "2020-01-02"), tz = "UTC"))
    ) |>
      collect(),
    tibble(date = c("2020-01-01", "2020-01-02"))
  )
})

test_that("can translate blob columns", {
  con <- local_sqlite_connection()

  df <- tibble(x = blob::blob(charToRaw("abc"), charToRaw("def")))
  db <- copy_inline(con, df)

  expect_snapshot(show_query(db))
  expect_equal(collect(db), df)
})

test_that("can translate 1-column tables", {
  con <- local_sqlite_connection()
  expect_snapshot(
    copy_inline(con, tibble(dbl = 1.5)) |>
      remote_query()
  )
})

test_that("zero row table works", {
  con <- local_sqlite_connection()
  expect_snapshot(
    copy_inline(con, tibble(dbl = numeric(), chr = character())) |>
      remote_query()
  )

  expect_snapshot(
    copy_inline(con, tibble(dbl = numeric())) |>
      remote_query()
  )
})

test_that("types argument works", {
  con <- local_sqlite_connection()

  df <- tibble(x = "1", y = 2L)
  expect_equal(copy_inline(con, df) |> collect(), df)

  expect_equal(
    copy_inline(con, df, types = c(x = "INTEGER", y = "TEXT")) |> collect(),
    tibble(x = 1L, y = "2")
  )
})

test_that("checks inputs", {
  con <- dialect_ansi()

  expect_snapshot({
    (expect_error(copy_inline(con, tibble())))
    (expect_error(copy_inline(con, lazy_frame(a = 1))))

    (expect_error(copy_inline(con, tibble(a = 1), types = c(b = "bigint"))))
    (expect_error(copy_inline(con, tibble(a = 1), types = c(b = 1))))
  })
})
