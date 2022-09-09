test_that("logicals translated to integers", {
  expect_equal(escape(FALSE, con = simulate_sqlite()), sql("0"))
  expect_equal(escape(TRUE, con = simulate_sqlite()), sql("1"))
  expect_equal(escape(NA, con = simulate_sqlite()), sql("NULL"))
})

test_that("vectorised translations", {
  local_con(simulate_sqlite())

  expect_equal(translate_sql(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(translate_sql(paste0(x, y)), sql("`x` || `y`"))
})

test_that("pmin and max become MIN and MAX", {
  local_con(simulate_sqlite())

  expect_equal(translate_sql(pmin(x, y, na.rm = TRUE)), sql('MIN(`x`, `y`)'))
  expect_equal(translate_sql(pmax(x, y, na.rm = TRUE)), sql('MAX(`x`, `y`)'))
})

test_that("sqlite mimics two argument log", {
  local_con(simulate_sqlite())

  expect_equal(translate_sql(log(x)), sql('LOG(`x`)'))
  expect_equal(translate_sql(log(x, 10)), sql('LOG(`x`) / LOG(10.0)'))
})

test_that("date-time", {
  local_con(simulate_sqlite())

  expect_equal(translate_sql(today()), sql("DATE('now')"))
  expect_equal(translate_sql(now()), sql("DATETIME('now')"))
})

test_that("custom aggregates translated", {
  local_con(simulate_sqlite())

  expect_equal(translate_sql(median(x, na.rm = TRUE), window = FALSE), sql('MEDIAN(`x`)'))
  expect_equal(translate_sql(sd(x, na.rm = TRUE), window = FALSE), sql('STDEV(`x`)'))

  expect_snapshot({
    (expect_error(translate_sql(quantile(x, 0.5, na.rm = TRUE), window = FALSE)))
    (expect_error(translate_sql(quantile(x, 0.5, na.rm = TRUE), window = TRUE)))
  })
})

test_that("custom SQL translation", {
  lf <- lazy_frame(x = 1, con = simulate_sqlite())
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))
})

test_that("case_when translates correctly to ELSE when TRUE ~ is used", {
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE    ~ "undefined"
      ),
      con = simulate_sqlite()
    )
  )
})

test_that("full and right join", {
  df1 <- lazy_frame(
    x = 1:3, y = c("x", "y", "z"),
    con = simulate_sqlite()
  )
  df2 <- lazy_frame(
    x = c(1, 3, 5), y = c("a", "b", "c"), z = 11:13,
    con = simulate_sqlite()
  )

  expect_snapshot(full_join(df1, df2, by = "x"))
  expect_snapshot(right_join(df2, df1, by = "x"))
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

test_that("full and right join work", {
  df1 <- memdb_frame(x = 1:3, y = c("x", "y", "z"))
  df2 <- memdb_frame(x = c(1, 3, 5), y = c("a", "b", "c"), z = 11:13)

  expect_equal(
    collect(full_join(df1, df2, by = "x")),
    tibble(
      x = c(1, 2, 3, 5),
      y.x = c("x", "y", "z", NA),
      y.y = c("a", NA, "b", "c"),
      z = c(11, NA, 12, 13)
    )
  )

  expect_equal(
    collect(right_join(df2, df1, by = "x")),
    tibble(
      x = c(1, 2, 3),
      y.x = c("a", NA, "b"),
      z = c(11, NA, 12),
      y.y = c("x", "y", "z")
    )
  )
})
