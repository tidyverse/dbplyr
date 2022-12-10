test_that("custom scalar translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(bitwXor(x, 128L)),       sql("`x` # 128"))
  expect_equal(translate_sql(log10(x)),               sql("LOG(`x`)"))
  expect_equal(translate_sql(log(x)),                 sql("LN(`x`)"))
  expect_equal(translate_sql(log(x, 2)),              sql("LOG(`x`) / LOG(2.0)"))
  expect_equal(translate_sql(cot(x)),                 sql("1 / TAN(`x`)"))
  expect_equal(translate_sql(round(x, digits = 1.1)), sql("ROUND((`x`) :: numeric, 1)"))
  expect_equal(translate_sql(grepl("exp", x)),        sql("(`x`) ~ ('exp')"))
  expect_equal(translate_sql(grepl("exp", x, TRUE)),  sql("(`x`) ~* ('exp')"))
  expect_equal(translate_sql(substr("test", 2 , 3)),  sql("SUBSTR('test', 2, 2)"))
})

test_that("custom stringr functions translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(str_detect(x, y)), sql("`x` ~ `y`"))
  expect_equal(translate_sql(str_detect(x, y, negate = TRUE)), sql("!(`x` ~ `y`)"))
  expect_equal(translate_sql(str_replace(x, y, z)), sql("REGEXP_REPLACE(`x`, `y`, `z`)"))
  expect_equal(translate_sql(str_replace_all(x, y, z)), sql("REGEXP_REPLACE(`x`, `y`, `z`, 'g')"))
  expect_equal(translate_sql(str_squish(x)), sql("LTRIM(RTRIM(REGEXP_REPLACE(`x`, '\\s+', ' ', 'g')))"))
  expect_equal(translate_sql(str_remove(x, y)), sql("REGEXP_REPLACE(`x`, `y`, '')"))
  expect_equal(translate_sql(str_remove_all(x, y)), sql("REGEXP_REPLACE(`x`, `y`, '', 'g')"))
})

test_that("two variable aggregates are translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(cor(x, y), window = FALSE), sql("CORR(`x`, `y`)"))
  expect_equal(translate_sql(cor(x, y), window = TRUE),  sql("CORR(`x`, `y`) OVER ()"))
})

test_that("pasting translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(paste(x, y), window = FALSE),  sql("CONCAT_WS(' ', `x`, `y`)"))
  expect_equal(translate_sql(paste0(x, y), window = FALSE), sql("CONCAT_WS('', `x`, `y`)"))

  expect_error(translate_sql(paste0(x, collapse = ""), window = FALSE), "`collapse` not supported")
})

test_that("postgres mimics two argument log", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(log(x)), sql('LN(`x`)'))
  expect_equal(translate_sql(log(x, 10)), sql('LOG(`x`) / LOG(10.0)'))
  expect_equal(translate_sql(log(x, 10L)), sql('LOG(`x`) / LOG(10)'))
})

test_that("custom lubridate functions translated correctly", {
  local_con(simulate_postgres())

  expect_equal(translate_sql(day(x)), sql("EXTRACT(DAY FROM `x`)"))
  expect_equal(translate_sql(mday(x)), sql("EXTRACT(DAY FROM `x`)"))
  expect_equal(translate_sql(yday(x)), sql("EXTRACT(DOY FROM `x`)"))
  expect_equal(translate_sql(week(x)), sql("FLOOR((EXTRACT(DOY FROM `x`) - 1) / 7) + 1"))
  expect_equal(translate_sql(isoweek(x)), sql("EXTRACT(WEEK FROM `x`)"))
  expect_equal(translate_sql(quarter(x)), sql("EXTRACT(QUARTER FROM `x`)"))
  expect_equal(translate_sql(quarter(x, with_year = TRUE)), sql("(EXTRACT(YEAR FROM `x`) || '.' || EXTRACT(QUARTER FROM `x`))"))
  expect_error(translate_sql(quarter(x, fiscal_start = 2)))
  expect_equal(translate_sql(isoyear(x)), sql("EXTRACT(YEAR FROM `x`)"))

  expect_equal(translate_sql(seconds(x)), sql("CAST('`x` seconds' AS INTERVAL)"))
  expect_equal(translate_sql(minutes(x)), sql("CAST('`x` minutes' AS INTERVAL)"))
  expect_equal(translate_sql(hours(x)),   sql("CAST('`x` hours' AS INTERVAL)"))
  expect_equal(translate_sql(days(x)),    sql("CAST('`x` days' AS INTERVAL)"))
  expect_equal(translate_sql(weeks(x)),   sql("CAST('`x` weeks' AS INTERVAL)"))
  expect_equal(translate_sql(months(x)),  sql("CAST('`x` months' AS INTERVAL)"))
  expect_equal(translate_sql(years(x)),   sql("CAST('`x` years' AS INTERVAL)"))

  expect_equal(translate_sql(floor_date(x, 'month')),       sql("DATE_TRUNC('month', `x`)"))
  expect_equal(translate_sql(floor_date(x, 'week')),        sql("DATE_TRUNC('week', `x`)"))
})

test_that("custom SQL translation", {
  lf <- lazy_frame(x = 1, con = simulate_postgres())
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))

  con <- simulate_postgres()
  expect_snapshot(copy_inline(con, tibble(x = integer(), y = character())) %>% remote_query())
  expect_snapshot(copy_inline(con, tibble(x = 1:2, y = letters[1:2])) %>% remote_query())
})

test_that("`sql_query_insert()` works", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_postgres(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(error = TRUE,
    (sql_query_insert(
      con = simulate_postgres(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      conflict = "error",
      returning_cols = c("a", b2 = "b")
    ))
  )

  expect_snapshot(
    sql_query_insert(
      con = simulate_postgres(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      conflict = "ignore",
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("`sql_query_upsert()` with method = 'on_conflict' is correct", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_postgres(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_upsert(
      con = simulate_postgres(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      update_cols = c("c", "d"),
      returning_cols = c("a", b2 = "b"),
      method = "on_conflict"
    )
  )
})


# live database -----------------------------------------------------------

test_that("can explain", {
  db <- copy_to_test("postgres", data.frame(x = 1:3))
  expect_snapshot(db %>% mutate(y = x + 1) %>% explain())

  # `explain()` passes `...` to methods
  expect_snapshot(db %>% mutate(y = x + 1) %>% explain(format = "json"))
})

test_that("can overwrite temp tables", {
  src <- src_test("postgres")
  copy_to(src, mtcars, "mtcars", overwrite = TRUE)
  expect_error(copy_to(src, mtcars, "mtcars", overwrite = TRUE), NA)
})

test_that("copy_inline works", {
  src <- src_test("postgres")
  df <- tibble(
    lgl = TRUE,
    int = 1L,
    dbl = 1.5,
    chr = "a",
    date = as.Date("2020-01-01", tz = "UTC"),
    dtt = as.POSIXct("2020-01-01 01:23:45", tz = "UTC")
  )

  expect_equal(copy_inline(src, df) %>% collect(), df)
})

test_that("can insert with returning", {
  con <- src_test("postgres")

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- copy_to(con, df_x, "df_x", temporary = TRUE, overwrite = TRUE)
  withr::defer(DBI::dbRemoveTable(con, DBI::SQL("df_x")))

  df_y <- tibble(a = 1:3, b = 11:13, c = -(1:3), d = c("x", "y", "z"))
  y <- copy_to(con, df_y, "df_y", temporary = TRUE, overwrite = TRUE) %>%
    mutate(c = c + 1)
  withr::defer(DBI::dbRemoveTable(con, DBI::SQL("df_y")))

  # This errors because there is no unique constraint on (`a`, `b`)
  expect_snapshot(error = TRUE, {
    rows_insert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      conflict = "ignore",
      returning = everything(),
      method = "on_conflict"
    )
  }, transform = snap_transform_dbi)

  expect_error(
    rows_insert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      conflict = "ignore",
      returning = everything(),
      method = "where_not_exists"
    ),
    NA
  )

  x <- copy_to(con, df_x, "df_x", temporary = TRUE, overwrite = TRUE)
  db_create_index(con, "df_x", columns = c("a", "b"), unique = TRUE)

  expect_equal(
    rows_insert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      conflict = "ignore",
      returning = everything(),
      method = "on_conflict"
    ) %>%
      get_returned_rows(),
    tibble(
      a = 2:3,
      b = 12:13,
      c = c(-1L, -2L),
      d = c("y", "z")
    )
  )

  expect_equal(
    collect(x),
    tibble(a = 1:3, b = 11:13, c = c(1L, -1L, -2L), d = c("a", "y", "z"))
  )
})

test_that("casts `y` column for local df", {
  con <- src_test("postgres")

  DBI::dbWriteTable(
    con,
    "df_x",
    value = tibble(id = 1L, val = 10L, arr = "{1,2}"),
    field.types = c(id = "bigint", val = "bigint", arr = "integer[]")
  )
  withr::defer(DBI::dbRemoveTable(con, DBI::SQL("df_x")))

  y <- tibble(
    id = "2",
    val = 20,
    arr = "{1, 2, 3}"
  )

  out <- tibble(
    id = bit64::as.integer64(1:2),
    val = bit64::as.integer64(10L, 20L),
    arr = structure(c("{1,2}", "{1,2,3}"), class = "pq__int4")
  )
  expect_equal(
    rows_append(
      tbl(con, "df_x"),
      y,
      copy = TRUE,
      in_place = FALSE
    ) %>%
      collect(),
    out
  )

  rows_append(
    tbl(con, "df_x"),
    y,
    copy = TRUE,
    in_place = TRUE
  )

  expect_equal(tbl(con, "df_x") %>% collect(), out)
})

test_that("can upsert with returning", {
  con <- src_test("postgres")

  df_x <- tibble(a = 1:2, b = 11:12, c = 1:2, d = c("a", "b"))
  x <- copy_to(con, df_x, "df_x", temporary = TRUE, overwrite = TRUE)
  withr::defer(DBI::dbRemoveTable(con, DBI::SQL("df_x")))

  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- copy_to(con, df_y, "df_y", temporary = TRUE, overwrite = TRUE) %>%
    mutate(c = c + 1)
  withr::defer(DBI::dbRemoveTable(con, DBI::SQL("df_y")))

  # Errors because there is no unique index
  expect_snapshot(error = TRUE, {
    rows_upsert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      returning = everything(),
      method = "on_conflict"
    )
  }, transform = snap_transform_dbi)

  # DBI method does not need a unique index
  expect_error(
    rows_upsert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      returning = everything(),
      method = "cte_update"
    ),
    NA
  )

  x <- copy_to(con, df_x, "df_x", temporary = TRUE, overwrite = TRUE)
  db_create_index(con, "df_x", columns = c("a", "b"), unique = TRUE)

  expect_equal(
    rows_upsert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      returning = everything(),
      method = "on_conflict"
    ) %>%
      get_returned_rows() %>%
      arrange(a),
    tibble(
      a = 2:3,
      b = 12:13,
      c = c(-1L, -2L),
      d = c("y", "z")
    )
  )

  expect_equal(
    collect(x),
    tibble(a = 1:3, b = 11:13, c = c(1L, -1L, -2L), d = c("a", "y", "z"))
  )
})

test_that("correctly escapes dates", {
  con <- src_test("postgres")

  dd <- as.Date("2022-03-04")
  expect_equal(escape(dd, con = con), sql("'2022-03-04'::date"))
})
