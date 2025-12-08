test_that("defaults to postgres translations", {
  con <- simulate_redshift()
  expect_translation(con, log10(x), "LOG(`x`)")
})

test_that("string translations", {
  con <- simulate_redshift()

  expect_error(
    translate_sql(str_replace("xx", ".", "a"), con = con),
    class = "dbplyr_error_unsupported_fn"
  )
  expect_translation(
    con,
    str_replace_all("xx", ".", "a"),
    "REGEXP_REPLACE('xx', '.', 'a')"
  )

  expect_translation(con, substr(x, 2, 2), "SUBSTRING(`x`, 2, 1)")
  expect_translation(
    con,
    str_sub(x, 2, -2),
    "SUBSTRING(`x`, 2, LEN(`x`) - 2)"
  )

  expect_translation(con, paste("x", "y"), "'x' || ' ' || 'y'")
  expect_translation(con, paste0("x", "y"), "'x' || 'y'")
  expect_translation(con, str_c("x", "y"), "'x' || 'y'")
  expect_translation(con, str_ilike(x, y), "`x` ILIKE `y`")
})

test_that("numeric translations", {
  con <- simulate_redshift()

  expect_translation(con, as.numeric(x), "CAST(`x` AS FLOAT)")
  expect_translation(con, as.double(x), "CAST(`x` AS FLOAT)")
  expect_translation(
    con,
    round(1.234, 1),
    "ROUND((1.234) :: float, 1)"
  )
})

test_that("aggregate functions", {
  con <- simulate_redshift()

  expect_translation(
    con,
    str_flatten(x, y),
    "LISTAGG(`x`, `y`)",
    window = FALSE
  )
  expect_translation(
    con,
    str_flatten(x, y),
    "LISTAGG(`x`, `y`) OVER ()",
    window = TRUE
  )
  expect_translation(
    con,
    order_by(z, str_flatten(x, y)),
    "LISTAGG(`x`, `y`) WITHIN GROUP (ORDER BY `z`) OVER ()"
  )
})

test_that("lag and lead translation", {
  con <- simulate_redshift()

  expect_translation(con, lead(x), "LEAD(`x`, 1) OVER ()")
  expect_translation(con, lag(x), "LAG(`x`, 1) OVER ()")
})

test_that("copy_inline uses UNION ALL", {
  con <- simulate_redshift()
  y <- tibble::tibble(id = 1L, arr = "{1,2,3}")

  types <- c(id = "bigint", arr = "integer[]")
  expect_snapshot({
    copy_inline(con, y |> slice(0)) |> remote_query()
    copy_inline(con, y) |> remote_query()

    # with `types`
    copy_inline(con, y |> slice(0), types = types) |> remote_query()
    copy_inline(con, y, types = types) |> remote_query()
  })
})

test_that("custom clock functions translated correctly", {
  con <- simulate_redshift()
  expect_translation(
    con,
    add_years(x, 1),
    "DATEADD(YEAR, 1.0, `x`)"
  )
  expect_translation(
    con,
    add_days(x, 1),
    "DATEADD(DAY, 1.0, `x`)"
  )
  expect_error(
    translate_sql(add_days(x, 1, "dots", "must", "be empty"), con = con),
    class = "rlib_error_dots_nonempty"
  )
  expect_translation(
    con,
    date_build(2020, 1, 1),
    "TO_DATE(CAST(2020.0 AS TEXT) || '-' || CAST(1.0 AS TEXT) || '-' || CAST(1.0 AS TEXT), 'YYYY-MM-DD')"
  )
  expect_translation(
    con,
    date_build(year_column, 1L, 1L),
    "TO_DATE(CAST(`year_column` AS TEXT) || '-' || CAST(1 AS TEXT) || '-' || CAST(1 AS TEXT), 'YYYY-MM-DD')"
  )
  expect_translation(
    con,
    get_year(date_column),
    "DATE_PART('year', `date_column`)"
  )
  expect_translation(
    con,
    get_month(date_column),
    "DATE_PART('month', `date_column`)"
  )
  expect_translation(
    con,
    get_day(date_column),
    "DATE_PART('day', `date_column`)"
  )
  expect_translation(
    con,
    date_count_between(date_column_1, date_column_2, "day"),
    "DATEDIFF(DAY, `date_column_1`, `date_column_2`)"
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(
      date_count_between(date_column_1, date_column_2, "year"),
      con = con
    )
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(
      date_count_between(
        date_column_1,
        date_column_2,
        "day",
        n = 5
      ),
      con = con
    )
  )
})

test_that("difftime is translated correctly", {
  con <- simulate_redshift()
  expect_translation(
    con,
    difftime(start_date, end_date, units = "days"),
    "DATEDIFF(DAY, `end_date`, `start_date`)"
  )
  expect_translation(
    con,
    difftime(start_date, end_date),
    "DATEDIFF(DAY, `end_date`, `start_date`)"
  )

  expect_snapshot(
    error = TRUE,
    translate_sql(difftime(start_date, end_date, units = "auto"), con = con)
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(
      difftime(
        start_date,
        end_date,
        tz = "UTC",
        units = "days"
      ),
      con = con
    )
  )
})

test_that("window functions with redshift specific error message", {
  con <- simulate_redshift()
  expect_error(
    translate_sql(quantile(x, 0.3, na.rm = TRUE), window = TRUE, con = con),
    "Redshift"
  )
  expect_error(
    translate_sql(median(x, na.rm = TRUE), window = TRUE, con = con),
    "Redshift"
  )
})
