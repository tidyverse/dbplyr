test_that("table_path_components parses SQL Server bracket syntax", {
  components <- function(x) {
    table_path_components(table_path(x), dialect_mssql())
  }

  # Bracket quoting
  expect_equal(components("[df]"), list("df"))
  expect_equal(components("[schema].[table]"), list(c("schema", "table")))
  expect_equal(components("[x.y].[z]"), list(c("x.y", "z")))
  expect_equal(components(c("[a]", "[b].[c]")), list("a", c("b", "c")))

  # Double-quote quoting
  expect_equal(components('"df"'), list("df"))
  expect_equal(components('"schema"."table"'), list(c("schema", "table")))
  expect_equal(components('"x.y"."z"'), list(c("x.y", "z")))
})

# function translation ----------------------------------------------------

test_that("custom scalar translated correctly", {
  con <- dialect_mssql()

  expect_translation(con, as.logical(x), "TRY_CAST([x] AS BIT)")
  expect_translation(con, as.numeric(x), "TRY_CAST([x] AS FLOAT)")
  expect_translation(
    con,
    as.integer(x),
    "TRY_CAST(TRY_CAST([x] AS NUMERIC) AS INT)"
  )
  expect_translation(
    con,
    as.integer64(x),
    "TRY_CAST(TRY_CAST([x] AS NUMERIC(38, 0)) AS BIGINT)"
  )
  expect_translation(con, as.double(x), "TRY_CAST([x] AS FLOAT)")
  expect_translation(con, as.character(x), "TRY_CAST([x] AS VARCHAR(MAX))")
  expect_translation(con, log(x), "LOG([x])")
  expect_translation(con, nchar(x), "LEN([x])")
  expect_translation(con, atan2(x), "ATN2([x])")
  expect_translation(con, ceiling(x), "CEILING([x])")
  expect_translation(con, ceil(x), "CEILING([x])")
  expect_translation(con, substr(x, 1, 2), "SUBSTRING([x], 1, 2)")
  expect_translation(con, trimws(x), "LTRIM(RTRIM([x]))")
  expect_translation(con, paste(x, y), "[x] + ' ' + [y]")

  expect_error(
    translate_sql(bitwShiftL(x, 2L), con = con),
    class = "dbplyr_error_unsupported_fn"
  )
  expect_error(
    translate_sql(bitwShiftR(x, 2L), con = con),
    class = "dbplyr_error_unsupported_fn"
  )
})

test_that("contents of [ have bool context", {
  con <- dialect_mssql()
  local_context(list(clause = "SELECT"))

  expect_translation(con, x[x > y], "CASE WHEN ([x] > [y]) THEN ([x]) END")
})

test_that("custom stringr functions translated correctly", {
  con <- dialect_mssql()

  expect_translation(con, str_length(x), "LEN([x])")
})

test_that("stringr fixed patterns use CHARINDEX", {
  con <- dialect_mssql()

  expect_translation(
    con,
    str_detect(x, fixed("abc")),
    "CHARINDEX('abc', [x]) > 0"
  )
  expect_translation(
    con,
    str_detect(x, fixed("abc"), negate = TRUE),
    "CHARINDEX('abc', [x]) = 0"
  )
  expect_translation(
    con,
    str_starts(x, fixed("abc")),
    "CHARINDEX('abc', [x]) = 1"
  )
  expect_translation(
    con,
    str_ends(x, fixed("abc")),
    "CHARINDEX('abc', [x]) = (LEN([x]) - LEN('abc')) + 1"
  )
})

test_that("stringr regex patterns require SQL Server 2025", {
  con <- dialect_mssql("15.0")
  expect_error(
    translate_sql(str_detect(x, "abc"), con = con),
    "Only fixed patterns are supported"
  )
})

test_that("stringr regex functions work on SQL Server 2025", {
  con <- dialect_mssql("17.0")

  expect_translation(con, str_detect(x, "abc"), "REGEXP_LIKE([x], 'abc')")
  expect_translation(
    con,
    str_detect(x, "abc", negate = TRUE),
    "NOT REGEXP_LIKE([x], 'abc')"
  )
  expect_translation(
    con,
    str_starts(x, "abc"),
    "REGEXP_LIKE([x], '^' + 'abc')"
  )
  expect_translation(
    con,
    str_ends(x, "abc"),
    "REGEXP_LIKE([x], 'abc' + '$')"
  )
  expect_translation(
    con,
    str_replace(x, "abc", "def"),
    "REGEXP_REPLACE([x], 'abc', 'def', 1, 1)"
  )
  expect_translation(
    con,
    str_replace_all(x, "abc", "def"),
    "REGEXP_REPLACE([x], 'abc', 'def')"
  )
  expect_translation(
    con,
    str_remove(x, "abc"),
    "REGEXP_REPLACE([x], 'abc', '', 1, 1)"
  )
  expect_translation(
    con,
    str_remove_all(x, "abc"),
    "REGEXP_REPLACE([x], 'abc', '')"
  )
  expect_translation(con, str_extract(x, "abc"), "REGEXP_SUBSTR([x], 'abc')")
  expect_translation(con, str_count(x, "abc"), "REGEXP_COUNT([x], 'abc')")
})

test_that("str_detect returns bit in SELECT context on SQL Server 2025", {
  lf <- lazy_frame(x = "test", con = dialect_mssql("17.0"))

  expect_snapshot(lf |> mutate(detected = str_detect(x, "abc")))
  expect_snapshot(lf |> filter(str_detect(x, "abc")))
})

test_that("custom aggregators translated correctly", {
  con <- dialect_mssql()

  expect_translation(con, sd(x, na.rm = TRUE), "STDEV([x])", window = FALSE)
  expect_translation(con, var(x, na.rm = TRUE), "VAR([x])", window = FALSE)

  expect_error(
    translate_sql(cor(x), con = con, window = FALSE),
    class = "dbplyr_error_unsupported_fn"
  )
  expect_error(
    translate_sql(cov(x), con = con, window = FALSE),
    class = "dbplyr_error_unsupported_fn"
  )

  expect_translation(con, str_flatten(x), "STRING_AGG([x], '')", window = FALSE)
  expect_snapshot(error = TRUE, {
    translate_sql(quantile(x, 0.5, na.rm = TRUE), con = con, window = FALSE)
    translate_sql(median(x, na.rm = TRUE), con = con, window = FALSE)
  })

  expect_translation(
    con,
    all(x, na.rm = TRUE),
    "CAST(MIN(CAST([x] AS INT)) AS BIT)",
    window = FALSE
  )
  expect_translation(
    con,
    any(x, na.rm = TRUE),
    "CAST(MAX(CAST([x] AS INT)) AS BIT)",
    window = FALSE
  )
})

test_that("custom window functions translated correctly", {
  con <- dialect_mssql()

  expect_translation(con, sd(x, na.rm = TRUE), "STDEV([x]) OVER ()")
  expect_translation(con, var(x, na.rm = TRUE), "VAR([x]) OVER ()")

  expect_translation(con, str_flatten(x), "STRING_AGG([x], '') OVER ()")

  expect_translation(
    con,
    quantile(x, 0.3, na.rm = TRUE),
    "PERCENTILE_CONT(0.3) WITHIN GROUP (ORDER BY [x]) OVER ()",
    window = TRUE
  )
  expect_translation(
    con,
    median(x, na.rm = TRUE),
    "PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY [x]) OVER ()",
    window = TRUE
  )

  expect_translation(
    con,
    all(x, na.rm = TRUE),
    "CAST(MIN(CAST([x] AS INT)) OVER () AS BIT)"
  )
  expect_translation(
    con,
    any(x, na.rm = TRUE),
    "CAST(MAX(CAST([x] AS INT)) OVER () AS BIT)"
  )

  expect_snapshot(
    translate_sql(n_distinct(x), con = con, vars_group = "x"),
    error = TRUE
  )
})

test_that("custom lubridate functions translated correctly", {
  con <- dialect_mssql()
  expect_translation(con, as_date(x), "TRY_CAST([x] AS DATE)")
  expect_translation(con, as_datetime(x), "TRY_CAST([x] AS DATETIME2)")
  expect_translation(con, today(), "CAST(SYSDATETIME() AS DATE)")
  expect_translation(con, year(x), "DATEPART(YEAR, [x])")
  expect_translation(con, day(x), "DATEPART(DAY, [x])")
  expect_translation(con, mday(x), "DATEPART(DAY, [x])")
  expect_translation(con, yday(x), "DATEPART(DAYOFYEAR, [x])")
  expect_translation(con, hour(x), "DATEPART(HOUR, [x])")
  expect_translation(con, minute(x), "DATEPART(MINUTE, [x])")
  expect_translation(con, second(x), "DATEPART(SECOND, [x])")
  expect_translation(con, month(x), "DATEPART(MONTH, [x])")
  expect_translation(
    con,
    month(x, label = TRUE, abbr = FALSE),
    "DATENAME(MONTH, [x])"
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(month(x, label = TRUE, abbr = TRUE), con = con)
  )

  expect_translation(con, quarter(x), "DATEPART(QUARTER, [x])")
  expect_translation(
    con,
    quarter(x, with_year = TRUE),
    "(DATENAME(YEAR, [x]) + '.' + DATENAME(QUARTER, [x]))"
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(quarter(x, fiscal_start = 5), con = con)
  )
})

test_that("custom clock functions translated correctly", {
  con <- dialect_mssql()
  expect_translation(con, add_years(x, 1), "DATEADD(YEAR, 1.0, [x])")
  expect_translation(con, add_days(x, 1), "DATEADD(DAY, 1.0, [x])")
  expect_error(
    translate_sql(add_days(x, 1, "dots", "must", "be empty"), con = con),
    class = "rlib_error_dots_nonempty"
  )
  expect_translation(
    con,
    date_build(2020, 1, 1),
    "DATEFROMPARTS(2020.0, 1.0, 1.0)"
  )
  expect_translation(
    con,
    date_build(year_column, 1L, 1L),
    "DATEFROMPARTS([year_column], 1, 1)"
  )
  expect_translation(
    con,
    get_year(date_column),
    "DATEPART(YEAR, [date_column])"
  )
  expect_translation(
    con,
    get_month(date_column),
    "DATEPART(MONTH, [date_column])"
  )
  expect_translation(con, get_day(date_column), "DATEPART(DAY, [date_column])")
  expect_translation(
    con,
    date_count_between(date_column_1, date_column_2, "day"),
    "DATEDIFF_BIG(DAY, [date_column_1], [date_column_2])"
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
  con <- dialect_mssql()
  expect_translation(
    con,
    difftime(start_date, end_date, units = "days"),
    "DATEDIFF_BIG(DAY, [end_date], [start_date])"
  )
  expect_translation(
    con,
    difftime(start_date, end_date),
    "DATEDIFF_BIG(DAY, [end_date], [start_date])"
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

test_that("last_value_sql() translated correctly", {
  con <- dialect_mssql()
  expect_equal(
    translate_sql(last(x, na_rm = TRUE), vars_order = "a", con = con),
    sql(
      "LAST_VALUE([x]) IGNORE NULLS OVER (ORDER BY [a] ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)"
    )
  )
})

test_that("between translation respects context", {
  con <- dialect_mssql()

  local_context(list(clause = "WHERE"))
  expect_translation(con, between(a, 1L, 2L), "[a] BETWEEN 1 AND 2")
  local_context(list(clause = "SELECT"))
  expect_translation(con, between(a, 1L, 2L), "IIF([a] BETWEEN 1 AND 2, 1, 0)")
})

# verb translation --------------------------------------------------------

test_that("convert between bit and boolean as needed", {
  mf <- lazy_frame(x = 1, con = dialect_mssql())

  # No conversion
  expect_snapshot(mf |> filter(is.na(x)))
  expect_snapshot(mf |> filter(!is.na(x)))
  expect_snapshot(mf |> filter(x == 1L || x == 2L))
  expect_snapshot(mf |> mutate(z = ifelse(x == 1L, 1L, 2L)))
  expect_snapshot(mf |> mutate(z = case_when(x == 1L ~ 1L)))

  # Single conversion on outer layer
  expect_snapshot(mf |> mutate(z = !is.na(x)))
  expect_snapshot(mf |> mutate(x = x == 1L))
  expect_snapshot(mf |> mutate(x = x == 1L || x == 2L))
  expect_snapshot(mf |> mutate(x = x == 1L || x == 2L || x == 3L))
  expect_snapshot(mf |> mutate(x = !(x == 1L || x == 2L || x == 3L)))
})

test_that("handles ORDER BY in subqueries", {
  expect_snapshot(
    sql_query_select(
      dialect_mssql(),
      sql("[x]"),
      sql("[y]"),
      order_by = "z",
      subquery = TRUE
    )
  )
})

test_that("custom limit translation", {
  expect_snapshot(
    sql_query_select(
      dialect_mssql(),
      sql("[x]"),
      sql("[y]"),
      order_by = sql("[z]"),
      limit = 10
    )
  )
})

test_that("custom escapes translated correctly", {
  mf <- lazy_frame(x = "abc", con = dialect_mssql())

  a <- blob::as_blob("abc")
  b <- blob::as_blob(as.raw(c(0x01, 0x02)))
  L <- c(a, b)

  expect_snapshot(mf |> filter(x == a))
  expect_snapshot(mf |> filter(x %in% L))

  # expect_snapshot() also uses !!
  qry <- mf |> filter(x %in% !!L)
  expect_snapshot(qry)
})

test_that("logical escaping to 0/1 for both filter() and mutate()", {
  mf <- lazy_frame(x = "abc", con = dialect_mssql())
  expect_snapshot(mf |> filter(x == TRUE))
  expect_snapshot(mf |> mutate(x = TRUE))
})

test_that("sql_escape_raw handles NULLs", {
  con <- dialect_mssql()
  expect_equal(sql_escape_raw(con, NULL), "NULL")
})

test_that("generates custom sql", {
  con <- dialect_mssql()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))

  # Creates the same SQL since there's no temporary CLAUSE
  # Automatic renaming is handled upstream by db_collect()/db_copy_to()
  expect_snapshot(sql_query_save(
    con,
    sql("SELECT * FROM foo"),
    in_schema("schema", "tbl")
  ))
  expect_snapshot(sql_query_save(
    con,
    sql("SELECT * FROM foo"),
    in_schema("schema", "tbl"),
    temporary = FALSE
  ))

  lf <- lazy_frame(x = 1:3, con = dialect_mssql())
  expect_snapshot(lf |> slice_sample(n = 1))

  expect_snapshot(
    copy_inline(con, tibble(x = 1:2, y = letters[1:2])) |> remote_query()
  )
  expect_snapshot(copy_inline(con, trees) |> remote_query())
})

test_that("`sql_query_insert()` is correct", {
  con <- dialect_mssql()
  df_y <- lazy_frame(
    a = 2:3,
    b = c(12L, 13L),
    c = -(2:3),
    d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) |>
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_insert(
      con = con,
      table = ident("df_x"),
      from = sql_render(df_y, con, lvl = 1),
      insert_cols = colnames(df_y),
      by = c("a", "b"),
      conflict = "ignore",
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("`sql_query_append()` is correct", {
  con <- dialect_mssql()
  df_y <- lazy_frame(
    a = 2:3,
    b = c(12L, 13L),
    c = -(2:3),
    d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) |>
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_append(
      con = con,
      table = ident("df_x"),
      from = sql_render(df_y, con, lvl = 1),
      insert_cols = colnames(df_y),
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("`sql_query_update_from()` is correct", {
  con <- dialect_mssql()
  df_y <- lazy_frame(
    a = 2:3,
    b = c(12L, 13L),
    c = -(2:3),
    d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) |>
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_update_from(
      con = con,
      table = ident("df_x"),
      from = sql_render(df_y, con, lvl = 1),
      by = c("a", "b"),
      update_values = sql(
        c = "COALESCE([df_x].[c], [...y].[c])",
        d = "[...y].[d]"
      ),
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("`sql_query_delete()` is correct", {
  df_y <- lazy_frame(
    a = 2:3,
    b = c(12L, 13L),
    c = -(2:3),
    d = c("y", "z"),
    con = dialect_mssql(),
    .name = "df_y"
  ) |>
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_delete(
      con = dialect_mssql(),
      table = ident("df_x"),
      from = sql_render(df_y, dialect_mssql(), lvl = 2),
      by = c("a", "b"),
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("`sql_query_upsert()` is correct", {
  con <- dialect_mssql()
  df_y <- lazy_frame(
    a = 2:3,
    b = c(12L, 13L),
    c = -(2:3),
    d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) |>
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_upsert(
      con = con,
      table = ident("df_x"),
      from = sql_render(df_y, con, lvl = 1),
      by = c("a", "b"),
      update_cols = c("c", "d"),
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("atoms and symbols are cast to bit in `filter`", {
  mf <- lazy_frame(x = TRUE, con = dialect_mssql())

  # as simple symbol and atom
  expect_snapshot(mf |> filter(x))
  expect_snapshot(mf |> filter(TRUE))

  # when involved in a (perhaps nested) logical expression
  expect_snapshot(mf |> filter((!x) | FALSE))

  # in a subquery
  expect_snapshot(mf |> filter(x) |> inner_join(mf, by = "x"))
})

test_that("row_number() with and without group_by() and arrange(): unordered defaults to Ordering by NULL (per empty_order)", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = dialect_mssql())
  expect_snapshot(mf |> mutate(rown = row_number()))
  expect_snapshot(mf |> group_by(y) |> mutate(rown = row_number()))
  expect_snapshot(mf |> arrange(y) |> mutate(rown = row_number()))
})

test_that("count_big", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = dialect_mssql())
  expect_snapshot(count(mf))
})

# Live database -----------------------------------------------------------

test_that("can copy_to() and compute() with temporary tables (#438)", {
  con <- test_mssql()
  df <- tibble(x = 1:3)

  # converts to name automatically with message
  expect_snapshot(
    db <- copy_to(con, df, name = unique_table_name(), temporary = TRUE),
    transform = snap_transform_dbi
  )
  expect_equal(db |> pull(), 1:3)

  expect_snapshot(
    db2 <- db |> mutate(y = x + 1) |> compute(),
    transform = snap_transform_dbi
  )
  expect_equal(db2 |> pull(), 2:4)
})

test_that("add prefix to temporary table", {
  con <- dialect_mssql()
  expect_snapshot(
    out <- sql_table_temporary(con, table_path("foo.bar"), temporary = TRUE)
  )
  expect_equal(out, list(table = table_path("[foo].[#bar]"), temporary = FALSE))

  expect_silent(
    out <- sql_table_temporary(con, table_path("foo.#bar"), temporary = TRUE)
  )
  expect_equal(out, list(table = table_path("foo.#bar"), temporary = FALSE))
})

test_that("bit conversion works for important cases", {
  df <- tibble(x = 1:3, y = 3:1)
  db <- copy_to(test_mssql(), df, name = unique_table_name("#"))
  expect_equal(db |> mutate(z = x == y) |> pull(), c(FALSE, TRUE, FALSE))
  expect_equal(db |> filter(x == y) |> pull(), 2)

  df <- tibble(x = c(TRUE, FALSE, FALSE), y = c(TRUE, FALSE, TRUE))
  db <- copy_to(test_mssql(), df, name = unique_table_name("#"))
  expect_equal(db |> filter(x == 1) |> pull(), TRUE)
  expect_equal(db |> mutate(z = TRUE) |> pull(), c(1, 1, 1))

  # Currently not supported: would need to determine that we have a bit
  # vector in a boolean context, and convert to boolean with x == 1.
  # expect_equal(db |> mutate(z = x) |> pull(), c(TRUE, FALSE, FALSE))
  # expect_equal(db |> mutate(z = !x) |> pull(), c(FALSE, TRUE, TRUE))
  # expect_equal(db |> mutate(z = x & y) |> pull(), c(TRUE, FALSE, FALSE))
})

test_that("as.integer and as.integer64 translations if parsing failures", {
  df <- data.frame(x = c("1.3", "2x"))
  db <- copy_to(test_mssql(), df, name = unique_table_name("#"))

  out <- db |>
    mutate(
      integer = as.integer(x),
      integer64 = as.integer64(x),
      numeric = as.numeric(x),
    ) |>
    collect()

  expect_identical(out$integer, c(1L, NA))
  expect_identical(out$integer64, bit64::as.integer64(c(1L, NA)))
  expect_identical(out$numeric, c(1.3, NA))
})

test_that("can insert", {
  con <- test_mssql()

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y", temporary = TRUE, overwrite = TRUE) |>
    mutate(c = c + 1)

  expect_equal(
    rows_insert(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE,
      conflict = "ignore"
    ) |>
      collect(),
    tibble(
      a = 1:3,
      b = 11:13,
      c = c(1L, -1L, -2L),
      d = c("a", "y", "z")
    )
  )
})

test_that("can insert with returning", {
  con <- test_mssql()

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  expect_equal(
    rows_insert(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE,
      conflict = "ignore",
      returning = everything()
    ) |>
      get_returned_rows(),
    tibble(
      a = 2:3,
      b = 12:13,
      c = c(-1L, -2L),
      d = c("y", "z")
    )
  )
})

test_that("can append", {
  con <- test_mssql()

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 1:3, b = 11:13, c = -(2:4), d = c("y", "z", "w"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  expect_equal(
    rows_append(
      x,
      y,
      in_place = TRUE
    ) |>
      collect(),
    tibble(
      a = c(1L, 1:3),
      b = c(11L, 11:13),
      c = c(1L, -1L, -2L, -3L),
      d = c("a", "y", "z", "w")
    )
  )
})

test_that("can append with returning", {
  con <- test_mssql()

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 1:3, b = 11:13, c = -(2:4), d = c("y", "z", "w"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  expect_equal(
    rows_append(
      x,
      y,
      in_place = TRUE,
      returning = everything()
    ) |>
      get_returned_rows(),
    tibble(
      a = 1:3,
      b = 11:13,
      c = c(-1L, -2L, -3L),
      d = c("y", "z", "w")
    )
  )
})

test_that("can update", {
  con <- test_mssql()

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  expect_equal(
    rows_update(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore"
    ) |>
      collect(),
    tibble(
      a = 1:3,
      b = 11:13,
      c = c(1L, -1L, -2L),
      d = c("a", "y", "z")
    )
  )
})

test_that("can update with returning", {
  con <- test_mssql()

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  expect_equal(
    rows_update(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore",
      returning = everything()
    ) |>
      get_returned_rows(),
    tibble(
      a = 2:3,
      b = 12:13,
      c = c(-1L, -2L),
      d = c("y", "z")
    )
  )
})

test_that("can upsert", {
  con <- test_mssql()

  df_x <- tibble(a = 1:2, b = 11:12, c = 1:2, d = c("a", "b"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  expect_equal(
    rows_upsert(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE
    ) |>
      collect(),
    tibble(
      a = 1:3,
      b = 11:13,
      c = c(1L, -1L, -2L),
      d = c("a", "y", "z")
    )
  )
})

test_that("can upsert with returning", {
  con <- test_mssql()

  df_x <- tibble(a = 1:2, b = 11:12, c = 1:2, d = c("a", "b"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  expect_equal(
    rows_upsert(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE,
      returning = everything()
    ) |>
      get_returned_rows() |>
      arrange(a),
    tibble(
      a = 2:3,
      b = 12:13,
      c = c(-1L, -2L),
      d = c("y", "z")
    )
  )
})

test_that("can delete", {
  con <- test_mssql()

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L))
  y <- local_db_table(con, df_y, "df_y")

  expect_equal(
    rows_delete(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore"
    ) |>
      collect(),
    tibble(
      a = 1L,
      b = 11L,
      c = 1L,
      d = "a"
    )
  )
})

test_that("can delete with returning", {
  con <- test_mssql()

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L))
  y <- local_db_table(con, df_y, "df_y")

  expect_equal(
    rows_delete(
      x,
      y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore",
      returning = everything()
    ) |>
      get_returned_rows(),
    tibble(
      a = 2:3,
      b = 12:13,
      c = 2:3,
      d = c("b", "c")
    )
  )
})
