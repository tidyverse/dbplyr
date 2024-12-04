test_that("logicals translated to integers", {
  expect_equal(escape(FALSE, con = simulate_sqlite()), sql("0"))
  expect_equal(escape(TRUE, con = simulate_sqlite()), sql("1"))
  expect_equal(escape(NA, con = simulate_sqlite()), sql("NULL"))
})

test_that("vectorised translations", {
  local_con(simulate_sqlite())

  expect_equal(test_translate_sql(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(test_translate_sql(paste0(x, y)), sql("`x` || `y`"))
})

test_that("pmin and max become MIN and MAX", {
  local_con(simulate_sqlite())

  expect_equal(test_translate_sql(pmin(x, y, na.rm = TRUE)), sql('MIN(`x`, `y`)'))
  expect_equal(test_translate_sql(pmax(x, y, na.rm = TRUE)), sql('MAX(`x`, `y`)'))
})

test_that("sqlite mimics two argument log", {
  local_con(simulate_sqlite())

  expect_equal(test_translate_sql(log(x)), sql('LOG(`x`)'))
  expect_equal(test_translate_sql(log(x, 10)), sql('LOG(`x`) / LOG(10.0)'))
})

test_that("date-time", {
  local_con(simulate_sqlite())

  expect_equal(test_translate_sql(today()), sql("DATE('now')"))
  expect_equal(test_translate_sql(now()), sql("DATETIME('now')"))
})

test_that("custom aggregates translated", {
  local_con(simulate_sqlite())

  expect_equal(test_translate_sql(median(x, na.rm = TRUE), window = FALSE), sql('MEDIAN(`x`)'))
  expect_equal(test_translate_sql(sd(x, na.rm = TRUE), window = FALSE), sql('STDEV(`x`)'))
  expect_error(
    test_translate_sql(quantile(x, 0.5, na.rm = TRUE), window = FALSE),
    class = "dbplyr_error_unsupported_fn"
  )
  expect_error(
    test_translate_sql(quantile(x, 0.5, na.rm = TRUE), window = TRUE),
    class = "dbplyr_error_unsupported_fn"
  )
})

test_that("custom SQL translation", {
  local_con(simulate_sqlite())

  lf <- lazy_frame(x = 1, con = simulate_sqlite())
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))

  expect_snapshot(test_translate_sql(runif(n())))
})

test_that("case_when translates correctly to ELSE when TRUE ~ is used", {
  expect_snapshot(
    test_translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE    ~ "undefined"
      ),
      con = simulate_sqlite()
    )
  )
})


# live database -----------------------------------------------------------

test_that("as.numeric()/as.double() get custom translation", {
  mf <- dbplyr::memdb_frame(x = 1L)

  out <- mf %>% mutate(x1 = as.numeric(x), x2 = as.double(x)) %>% collect()
  expect_type(out$x1, "double")
  expect_type(out$x2, "double")
})

test_that("date extraction agrees with R", {
  db <- memdb_frame(x = "2000-01-02 03:40:50.5")
  out <- db %>% transmute(
    year = year(x),
    month = month(x),
    day = day(x),
    hour = hour(x),
    minute = minute(x),
    second = second(x),
    yday = yday(x),
  ) %>% collect() %>% as.list()

  expect_equal(out, list(
    year = 2000,
    month = 1,
    day = 2,
    hour = 3,
    minute = 40,
    second = 50.5,
    yday = 2
  ))
})

test_that("can explain a query", {
  db <- copy_to_test("sqlite", data.frame(x = 1:5), indexes = list("x"))
  expect_snapshot(db %>% filter(x > 2) %>% explain())
})
