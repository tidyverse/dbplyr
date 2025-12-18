test_that("logicals translated to integers", {
  expect_equal(escape(FALSE, con = simulate_sqlite()), sql("0"))
  expect_equal(escape(TRUE, con = simulate_sqlite()), sql("1"))
  expect_equal(escape(NA, con = simulate_sqlite()), sql("NULL"))
})

test_that("vectorised translations", {
  con <- simulate_sqlite()

  expect_translation(con, paste(x, y), '`x` || \' \' || `y`')
  expect_translation(con, paste0(x, y), '`x` || `y`')
})

test_that("pmin and max become MIN and MAX", {
  con <- simulate_sqlite()

  expect_translation(con, pmin(x, y, na.rm = TRUE), 'MIN(`x`, `y`)')
  expect_translation(con, pmax(x, y, na.rm = TRUE), 'MAX(`x`, `y`)')
})

test_that("sqlite mimics two argument log", {
  con <- simulate_sqlite()

  expect_translation(con, log(x), 'LOG(`x`)')
  expect_translation(con, log(x, 10), 'LOG(`x`) / LOG(10.0)')
})

test_that("date-time", {
  con <- simulate_sqlite()

  expect_translation(con, today(), "DATE('now')")
  expect_translation(con, now(), "DATETIME('now')")
})

test_that("custom aggregates translated", {
  con <- simulate_sqlite()

  expect_translation(
    con,
    median(x, na.rm = TRUE),
    'MEDIAN(`x`)',
    window = FALSE
  )
  expect_translation(con, sd(x, na.rm = TRUE), 'STDEV(`x`)', window = FALSE)
  expect_error(
    translate_sql(quantile(x, 0.5, na.rm = TRUE), con = con, window = FALSE),
    class = "dbplyr_error_unsupported_fn"
  )
  expect_error(
    translate_sql(quantile(x, 0.5, na.rm = TRUE), con = con, window = TRUE),
    class = "dbplyr_error_unsupported_fn"
  )
})

test_that("custom SQL translation", {
  con <- simulate_sqlite()

  lf <- lazy_frame(x = 1, con = simulate_sqlite())
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))

  expect_snapshot(translate_sql(runif(n()), con = con))
})

test_that("case_when translates correctly to ELSE when TRUE ~ is used", {
  con <- simulate_sqlite()
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE ~ "undefined"
      ),
      con = con
    )
  )
})


# live database -----------------------------------------------------------

test_that("as.numeric()/as.double() get custom translation", {
  mf <- local_memdb_frame(x = 1L)

  out <- mf |> mutate(x1 = as.numeric(x), x2 = as.double(x)) |> collect()
  expect_type(out$x1, "double")
  expect_type(out$x2, "double")
})

test_that("date extraction agrees with R", {
  db <- local_memdb_frame(x = "2000-01-02 03:40:50.5")
  out <- db |>
    transmute(
      year = year(x),
      month = month(x),
      day = day(x),
      hour = hour(x),
      minute = minute(x),
      second = second(x),
      yday = yday(x),
    ) |>
    collect() |>
    as.list()

  expect_equal(
    out,
    list(
      year = 2000,
      month = 1,
      day = 2,
      hour = 3,
      minute = 40,
      second = 50.5,
      yday = 2
    )
  )
})

test_that("can explain a query", {
  db <- copy_to(
    test_sqlite(),
    data.frame(x = 1:5),
    name = "test",
    indexes = list("x"),
    overwrite = TRUE
  )
  expect_snapshot(db |> filter(x > 2) |> explain())
})
