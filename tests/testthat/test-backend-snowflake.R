test_that("custom scalar translated correctly", {
  con <- simulate_snowflake()
  expect_translation(con, log10(x), "LOG(10.0, `x`)")
  expect_translation(
    con,
    round(x, digits = 1.1),
    "ROUND((`x`) :: FLOAT, 1)"
  )
  expect_translation(
    con,
    grepl("exp", x),
    "REGEXP_INSTR(`x`, 'exp', 1, 1, 0, 'c') != 0"
  )

  expect_translation(
    con,
    grepl("exp", x, ignore.case = TRUE),
    "REGEXP_INSTR(`x`, 'exp', 1, 1, 0, 'i') != 0"
  )
})

test_that("pasting translated correctly", {
  con <- simulate_snowflake()

  expect_translation(
    con,
    paste(x, y),
    "ARRAY_TO_STRING(ARRAY_CONSTRUCT_COMPACT(`x`, `y`), ' ')"
  )
  expect_translation(
    con,
    paste0(x, y),
    "ARRAY_TO_STRING(ARRAY_CONSTRUCT_COMPACT(`x`, `y`), '')"
  )
  expect_translation(con, str_c(x, y), "CONCAT_WS('', `x`, `y`)")
  expect_translation(
    con,
    str_c(x, y, sep = "|"),
    "CONCAT_WS('|', `x`, `y`)"
  )

  expect_snapshot(
    error = TRUE,
    translate_sql(paste0(x, collapse = ""), con = con)
  )

  expect_translation(
    con,
    str_flatten(x),
    "LISTAGG(`x`, '') OVER ()",
    window = TRUE
  )
  expect_translation(
    con,
    str_flatten(x, collapse = "|"),
    "LISTAGG(`x`, '|') OVER ()",
    window = TRUE
  )
  expect_translation(
    con,
    str_flatten(x, collapse = "|"),
    "LISTAGG(`x`, '|')",
    window = FALSE
  )
})

test_that("custom stringr functions translated correctly", {
  con <- simulate_snowflake()

  expect_translation(con, str_locate(x, y), "POSITION(`y`, `x`)")
  expect_translation(
    con,
    str_detect(x, y),
    "REGEXP_INSTR(`x`, `y`) != 0"
  )
  expect_translation(
    con,
    str_detect(x, y, negate = TRUE),
    "REGEXP_INSTR(`x`, `y`) = 0"
  )
  expect_translation(
    con,
    str_replace(x, y, z),
    "REGEXP_REPLACE(`x`, `y`, `z`, 1.0, 1.0)"
  )
  expect_translation(
    con,
    str_replace(x, "\\d", z),
    "REGEXP_REPLACE(`x`, '\\\\d', `z`, 1.0, 1.0)"
  )
  expect_translation(
    con,
    str_replace_all(x, y, z),
    "REGEXP_REPLACE(`x`, `y`, `z`)"
  )
  expect_translation(
    con,
    str_squish(x),
    "REGEXP_REPLACE(TRIM(`x`), '\\\\s+', ' ')"
  )
  expect_translation(
    con,
    str_remove(x, y),
    "REGEXP_REPLACE(`x`, `y`, '', 1.0, 1.0)"
  )
  expect_translation(
    con,
    str_remove_all(x, y),
    "REGEXP_REPLACE(`x`, `y`)"
  )
  expect_translation(con, str_trim(x), "TRIM(`x`)")
  expect_translation(
    con,
    str_starts(x, y),
    "REGEXP_INSTR(`x`, `y`) = 1"
  )
  expect_translation(
    con,
    str_starts(x, y, negate = TRUE),
    "REGEXP_INSTR(`x`, `y`) != 1"
  )
  expect_translation(
    con,
    str_ends(x, y),
    "REGEXP_INSTR(`x`, `y`, 1, 1, 1) = (LENGTH(`x`) + 1)"
  )
  expect_translation(
    con,
    str_ends(x, y, negate = TRUE),
    "REGEXP_INSTR(`x`, `y`, 1, 1, 1) != (LENGTH(`x`) + 1)"
  )

  expect_translation(con, str_like(x, y), "`x` LIKE `y`")
  expect_translation(con, str_ilike(x, y), "`x` ILIKE `y`")
})

test_that("aggregates are translated correctly", {
  con <- simulate_snowflake()

  expect_translation(
    con,
    cor(x, y),
    "CORR(`x`, `y`)",
    window = FALSE
  )
  expect_translation(
    con,
    cor(x, y),
    "CORR(`x`, `y`) OVER ()",
    window = TRUE
  )

  expect_translation(
    con,
    cov(x, y),
    "COVAR_SAMP(`x`, `y`)",
    window = FALSE
  )
  expect_translation(
    con,
    cov(x, y),
    "COVAR_SAMP(`x`, `y`) OVER ()",
    window = TRUE
  )

  expect_translation(
    con,
    all(x, na.rm = TRUE),
    "BOOLAND_AGG(`x`)",
    window = FALSE
  )
  expect_translation(
    con,
    all(x, na.rm = TRUE),
    "BOOLAND_AGG(`x`) OVER ()",
    window = TRUE
  )

  expect_translation(
    con,
    any(x, na.rm = TRUE),
    "BOOLOR_AGG(`x`)",
    window = FALSE
  )
  expect_translation(
    con,
    any(x, na.rm = TRUE),
    "BOOLOR_AGG(`x`) OVER ()",
    window = TRUE
  )

  expect_translation(
    con,
    sd(x, na.rm = TRUE),
    "STDDEV(`x`)",
    window = FALSE
  )
  expect_translation(
    con,
    sd(x, na.rm = TRUE),
    "STDDEV(`x`) OVER ()",
    window = TRUE
  )
})

test_that("snowflake mimics two argument log", {
  con <- simulate_snowflake()

  expect_translation(con, log(x), "LN(`x`)")
  expect_translation(con, log(x, 10), "LOG(10.0, `x`)")
  expect_translation(con, log(x, 10L), "LOG(10, `x`)")
})

test_that("custom lubridate functions translated correctly", {
  con <- simulate_snowflake()

  expect_translation(con, day(x), "EXTRACT(DAY FROM `x`)")
  expect_translation(con, mday(x), "EXTRACT(DAY FROM `x`)")
  expect_translation(con, yday(x), "EXTRACT('dayofyear', `x`)")
  expect_translation(
    con,
    wday(x),
    "EXTRACT('dayofweek', DATE(`x`) + 0) + 1.0"
  )
  expect_translation(con, wday(x, label = TRUE), "DAYNAME(`x`)")
  expect_translation(
    con,
    wday(x, label = TRUE, abbr = FALSE),
    "DECODE(EXTRACT('dayofweek', `x`), 1.0, 'Monday', 2.0, 'Tuesday', 3.0, 'Wednesday', 4.0, 'Thursday', 5.0, 'Friday', 6.0, 'Saturday', 0.0, 'Sunday')"
  )
  expect_translation(
    con,
    week(x),
    "FLOOR((EXTRACT('dayofyear', `x`) - 1) / 7) + 1"
  )
  expect_translation(con, isoweek(x), "EXTRACT('weekiso', `x`)")
  expect_translation(con, month(x), "EXTRACT('month', `x`)")
  expect_translation(
    con,
    month(x, label = TRUE),
    "MONTHNAME(`x`)"
  )
  expect_translation(
    con,
    month(x, label = TRUE, abbr = FALSE),
    "DECODE(EXTRACT('month', `x`), 1.0, 'January', 2.0, 'February', 3.0, 'March', 4.0, 'April', 5.0, 'May', 6.0, 'June', 7.0, 'July', 8.0, 'August', 9.0, 'September', 10.0, 'October', 11.0, 'November', 12.0, 'December')"
  )
  expect_translation(con, quarter(x), "EXTRACT('quarter', `x`)")
  expect_translation(
    con,
    quarter(x, with_year = TRUE),
    "(EXTRACT('year', `x`) || '.' || EXTRACT('quarter', `x`))"
  )
  expect_error(
    translate_sql(quarter(x, fiscal_start = 2), con = con),
    class = "dbplyr_error_unsupported_arg"
  )
  expect_translation(con, isoyear(x), "EXTRACT('year', `x`)")

  expect_translation(con, seconds(x), "INTERVAL '`x` second'")
  expect_translation(con, minutes(x), "INTERVAL '`x` minute'")
  expect_translation(con, hours(x), "INTERVAL '`x` hour'")
  expect_translation(con, days(x), "INTERVAL '`x` day'")
  expect_translation(con, weeks(x), "INTERVAL '`x` week'")
  expect_translation(con, months(x), "INTERVAL '`x` month'")
  expect_translation(con, years(x), "INTERVAL '`x` year'")

  expect_translation(
    con,
    floor_date(x, "month"),
    "DATE_TRUNC('month', `x`)"
  )
  expect_translation(
    con,
    floor_date(x, "week"),
    "DATE_TRUNC('week', `x`)"
  )
})

test_that("custom clock functions translated correctly", {
  con <- simulate_snowflake()
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
    "DATE_FROM_PARTS(2020.0, 1.0, 1.0)"
  )
  expect_translation(
    con,
    date_build(year_column, 1L, 1L),
    "DATE_FROM_PARTS(`year_column`, 1, 1)"
  )
  expect_translation(
    con,
    get_year(date_column),
    "DATE_PART(YEAR, `date_column`)"
  )
  expect_translation(
    con,
    get_month(date_column),
    "DATE_PART(MONTH, `date_column`)"
  )
  expect_translation(
    con,
    get_day(date_column),
    "DATE_PART(DAY, `date_column`)"
  )
  expect_translation(
    con,
    date_count_between(date_column_1, date_column_2, "day"),
    "DATEDIFF(DAY, `date_column_1`, `date_column_2`)"
  )
  expect_error(
    translate_sql(
      date_count_between(
        date_column_1,
        date_column_2,
        "year"
      ),
      con = con
    ),
    class = "dbplyr_error_unsupported_arg"
  )
  expect_error(
    translate_sql(
      date_count_between(
        date_column_1,
        date_column_2,
        "day",
        n = 5
      ),
      con = con
    ),
    class = "dbplyr_error_unsupported_arg"
  )
})

test_that("difftime is translated correctly", {
  con <- simulate_snowflake()
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

  expect_error(
    translate_sql(difftime(start_date, end_date, units = "auto"), con = con),
    class = "dbplyr_error_unsupported_arg"
  )
  expect_error(
    translate_sql(
      difftime(
        start_date,
        end_date,
        tz = "UTC",
        units = "days"
      ),
      con = con
    ),
    class = "dbplyr_error_unsupported_arg"
  )
})

test_that("min() and max()", {
  con <- simulate_snowflake()

  expect_translation(
    con,
    min(x, na.rm = TRUE),
    "MIN(`x`) OVER ()"
  )
  expect_translation(
    con,
    max(x, na.rm = TRUE),
    "MAX(`x`) OVER ()"
  )

  # na.rm = FALSE is ignored
  # https://docs.snowflake.com/en/sql-reference/functions/min
  # https://docs.snowflake.com/en/sql-reference/functions/max
  # NULL values are ignored unless all the records are NULL, in which case a NULL value is returned.
  expect_translation(
    con,
    min(x, na.rm = TRUE),
    "MIN(`x`) OVER ()"
  )

  expect_translation(
    con,
    max(x, na.rm = TRUE),
    "MAX(`x`) OVER ()"
  )
})

test_that("pmin() and pmax() respect na.rm", {
  con <- simulate_snowflake()

  # Snowflake default for LEAST/GREATEST: If any of the argument values is NULL, the result is NULL.
  # https://docs.snowflake.com/en/sql-reference/functions/least
  # https://docs.snowflake.com/en/sql-reference/functions/greatest

  # na.rm = TRUE: override default behavior for Snowflake (only supports pairs)
  expect_translation(
    con,
    pmin(x, y, na.rm = TRUE),
    "COALESCE(IFF(`x` <= `y`, `x`, `y`), `x`, `y`)"
  )
  expect_translation(
    con,
    pmax(x, y, na.rm = TRUE),
    "COALESCE(IFF(`x` >= `y`, `x`, `y`), `x`, `y`)"
  )

  expect_snapshot(translate_sql(pmin(x, y, z, na.rm = TRUE), con = con))
  expect_snapshot(translate_sql(pmax(x, y, z, na.rm = TRUE), con = con))

  # na.rm = FALSE: leverage default behavior for Snowflake
  expect_translation(
    con,
    pmin(x, y, z, na.rm = FALSE),
    "LEAST(`x`, `y`, `z`)"
  )
  expect_translation(
    con,
    pmax(x, y, z, na.rm = FALSE),
    "GREATEST(`x`, `y`, `z`)"
  )
})

test_that("row_number() with and without group_by() and arrange(): unordered defaults to Ordering by NULL (per empty_order)", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = simulate_snowflake())
  expect_snapshot(mf |> mutate(rown = row_number()))
  expect_snapshot(mf |> group_by(y) |> mutate(rown = row_number()))
  expect_snapshot(mf |> arrange(y) |> mutate(rown = row_number()))
})

test_that("correctly translates $", {
  con <- simulate_snowflake()
  expect_translation(con, x$y, "`x`:`y`")
})
