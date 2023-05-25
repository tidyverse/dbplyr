# function translation ----------------------------------------------------

test_that("custom scalar translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(as.logical(x)),   sql("TRY_CAST(`x` AS BIT)"))
  expect_equal(translate_sql(as.numeric(x)),   sql("TRY_CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(as.integer(x)),   sql("TRY_CAST(TRY_CAST(`x` AS NUMERIC) AS INT)"))
  expect_equal(translate_sql(as.integer64(x)), sql("TRY_CAST(TRY_CAST(`x` AS NUMERIC(38, 0)) AS BIGINT)"))
  expect_equal(translate_sql(as.double(x)),    sql("TRY_CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(as.character(x)), sql("TRY_CAST(`x` AS VARCHAR(MAX))"))
  expect_equal(translate_sql(log(x)),          sql("LOG(`x`)"))
  expect_equal(translate_sql(nchar(x)),        sql("LEN(`x`)"))
  expect_equal(translate_sql(atan2(x)),        sql("ATN2(`x`)"))
  expect_equal(translate_sql(ceiling(x)),      sql("CEILING(`x`)"))
  expect_equal(translate_sql(ceil(x)),         sql("CEILING(`x`)"))
  expect_equal(translate_sql(substr(x, 1, 2)), sql("SUBSTRING(`x`, 1, 2)"))
  expect_equal(translate_sql(trimws(x)),       sql("LTRIM(RTRIM(`x`))"))
  expect_equal(translate_sql(paste(x, y)),     sql("`x` + ' ' + `y`"))
  expect_equal(
    translate_sql(if_else(x, "true", "false", "missing")),
    sql("CASE WHEN `x` THEN 'true' WHEN NOT `x` THEN 'false' WHEN (`x` IS NULL) THEN 'missing' END")
  )
  expect_equal(
    translate_sql(ifelse(x, "true", "false")),
    sql("IIF(`x`, 'true', 'false')")
  )
  expect_equal(
    translate_sql(ifelse(x, "true", NULL)),
    sql("IIF(`x`, 'true', NULL)")
  )
  expect_equal(
    translate_sql(if(x) "true" else "false"),
    sql("IIF(`x`, 'true', 'false')")
  )

  expect_error(translate_sql(bitwShiftL(x, 2L)), sql("not available"))
  expect_error(translate_sql(bitwShiftR(x, 2L)), sql("not available"))
})

test_that("contents of [ have bool context", {
  local_con(simulate_mssql())
  local_context(list(clause = "SELECT"))

  expect_equal(translate_sql(x[x > y]), sql("CASE WHEN (`x` > `y`) THEN (`x`) END"))
})

test_that("custom stringr functions translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(str_length(x)),     sql("LEN(`x`)"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(sd(x, na.rm = TRUE), window = FALSE),  sql("STDEV(`x`)"))
  expect_equal(translate_sql(var(x, na.rm = TRUE), window = FALSE), sql("VAR(`x`)"))

  expect_error(translate_sql(cor(x), window = FALSE), "not available")
  expect_error(translate_sql(cov(x), window = FALSE), "not available")

  expect_equal(translate_sql(str_flatten(x), window = FALSE), sql("STRING_AGG(`x`, '')"))
  expect_snapshot(error = TRUE, {
    translate_sql(quantile(x, 0.5, na.rm = TRUE), window = FALSE)
    translate_sql(median(x, na.rm = TRUE), window = FALSE)
  })

  expect_equal(
    translate_sql(all(x, na.rm = TRUE), window = FALSE),
    sql("CAST(MIN(CAST(`x` AS INT)) AS BIT)")
  )
  expect_equal(
    translate_sql(any(x, na.rm = TRUE), window = FALSE),
    sql("CAST(MAX(CAST(`x` AS INT)) AS BIT)")
  )
})

test_that("custom window functions translated correctly", {
  local_con(simulate_mssql())

  expect_equal(translate_sql(sd(x, na.rm = TRUE)),  sql("STDEV(`x`) OVER ()"))
  expect_equal(translate_sql(var(x, na.rm = TRUE)), sql("VAR(`x`) OVER ()"))

  expect_equal(translate_sql(str_flatten(x)), sql("STRING_AGG(`x`, '') OVER ()"))

  expect_equal(
    translate_sql(quantile(x, 0.3, na.rm = TRUE), window = TRUE),
    sql("PERCENTILE_CONT(0.3) WITHIN GROUP (ORDER BY `x`) OVER ()")
  )
  expect_equal(
    translate_sql(median(x, na.rm = TRUE), window = TRUE),
    sql("PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY `x`) OVER ()")
  )

  expect_equal(
    translate_sql(all(x, na.rm = TRUE)),
    sql("CAST(MIN(CAST(`x` AS INT)) OVER () AS BIT)")
  )
  expect_equal(
    translate_sql(any(x, na.rm = TRUE)),
    sql("CAST(MAX(CAST(`x` AS INT)) OVER () AS BIT)")
  )
})

test_that("custom lubridate functions translated correctly", {
  local_con(simulate_mssql())
  expect_equal(translate_sql(as_date(x)),     sql("TRY_CAST(`x` AS DATE)"))
  expect_equal(translate_sql(as_datetime(x)), sql("TRY_CAST(`x` AS DATETIME2)"))
  expect_equal(translate_sql(today()),   sql("CAST(SYSDATETIME() AS DATE)"))
  expect_equal(translate_sql(year(x)),   sql("DATEPART(YEAR, `x`)"))
  expect_equal(translate_sql(day(x)),    sql("DATEPART(DAY, `x`)"))
  expect_equal(translate_sql(mday(x)),   sql("DATEPART(DAY, `x`)"))
  expect_equal(translate_sql(yday(x)),   sql("DATEPART(DAYOFYEAR, `x`)"))
  expect_equal(translate_sql(hour(x)),   sql("DATEPART(HOUR, `x`)"))
  expect_equal(translate_sql(minute(x)), sql("DATEPART(MINUTE, `x`)"))
  expect_equal(translate_sql(second(x)), sql("DATEPART(SECOND, `x`)"))
  expect_equal(translate_sql(month(x)), sql("DATEPART(MONTH, `x`)"))
  expect_equal(translate_sql(month(x, label = TRUE, abbr = FALSE)), sql("DATENAME(MONTH, `x`)"))
  expect_snapshot(error = TRUE, translate_sql(month(x, label = TRUE, abbr = TRUE)))

  expect_equal(translate_sql(quarter(x)), sql("DATEPART(QUARTER, `x`)"))
  expect_equal(translate_sql(quarter(x, with_year = TRUE)), sql("(DATENAME(YEAR, `x`) + '.' + DATENAME(QUARTER, `x`))"))
  expect_error(translate_sql(quarter(x, fiscal_start = 5)))
})

test_that("last_value_sql() translated correctly", {
  con <- simulate_mssql()
  expect_equal(
    translate_sql(last(x, na_rm = TRUE), vars_order = "a", con = con),
    sql("LAST_VALUE(`x`) IGNORE NULLS OVER (ORDER BY `a` ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)")
  )
})

# verb translation --------------------------------------------------------

test_that("convert between bit and boolean as needed", {
  mf <- lazy_frame(x = 1, con = simulate_mssql())

  # No conversion
  expect_snapshot(mf %>% filter(is.na(x)))
  expect_snapshot(mf %>% filter(!is.na(x)))
  expect_snapshot(mf %>% filter(x == 1L || x == 2L))
  expect_snapshot(mf %>% mutate(z = ifelse(x == 1L, 1L, 2L)))
  expect_snapshot(mf %>% mutate(z = case_when(x == 1L ~ 1L)))

  # Single conversion on outer layer
  expect_snapshot(mf %>% mutate(z = !is.na(x)))
  expect_snapshot(mf %>% mutate(x = x == 1L))
  expect_snapshot(mf %>% mutate(x = x == 1L || x == 2L))
  expect_snapshot(mf %>% mutate(x = x == 1L || x == 2L || x == 3L))
  expect_snapshot(mf %>% mutate(x = !(x == 1L || x == 2L || x == 3L)))
})

test_that("handles ORDER BY in subqueries", {
  expect_snapshot(
    sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = "z", subquery = TRUE)
  )
})

test_that("custom limit translation", {
  expect_snapshot(
    sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = ident("z"), limit = 10)
  )
})

test_that("custom escapes translated correctly", {
  mf <- lazy_frame(x = "abc", con = simulate_mssql())

  a <- blob::as_blob("abc")
  b <- blob::as_blob(as.raw(c(0x01, 0x02)))
  L <- c(a, b)

  expect_snapshot(mf %>% filter(x == a))
  expect_snapshot(mf %>% filter(x %in% L))

  # expect_snapshot() also uses !!
  qry <- mf %>% filter(x %in% !!L)
  expect_snapshot(qry)
})

test_that("logical escaping to 0/1 for both filter() and mutate()", {
  mf <- lazy_frame(x = "abc", con = simulate_mssql())
  expect_snapshot(mf %>% filter(x == TRUE))
  expect_snapshot(mf %>% mutate(x = TRUE))
})

test_that("sql_escape_raw handles NULLs", {
  con <- simulate_mssql()
  expect_equal(sql_escape_raw(con, NULL), "NULL")
})

test_that("generates custom sql", {
  con <- simulate_mssql()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))

  # Creates the same SQL since there's no temporary CLAUSE
  # Automatic renaming is handled upstream by db_collect()/db_copy_to()
  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl")))
  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"), temporary = FALSE))

  lf <- lazy_frame(x = 1:3, con = simulate_mssql())
  expect_snapshot(lf %>% slice_sample(n = 1))

  expect_snapshot(copy_inline(con, tibble(x = 1:2, y = letters[1:2])) %>% remote_query())
  expect_snapshot(copy_inline(con, trees) %>% remote_query())
})

test_that("`sql_query_insert()` is correct", {
  con <- simulate_mssql()
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) %>%
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
  con <- simulate_mssql()
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) %>%
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
  con <- simulate_mssql()
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_update_from(
      con = con,
      table = ident("df_x"),
      from = sql_render(df_y, con, lvl = 1),
      by = c("a", "b"),
      update_values = sql(
        c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"
      ),
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("`sql_query_delete()` is correct", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_mssql(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_delete(
      con = simulate_mssql(),
      table = ident("df_x"),
      from = sql_render(df_y, simulate_mssql(), lvl = 2),
      by = c("a", "b"),
      returning_cols = c("a", b2 = "b")
    )
  )
})

test_that("`sql_query_upsert()` is correct", {
  con <- simulate_mssql()
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = con,
    .name = "df_y"
  ) %>%
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
  mf <- lazy_frame(x = TRUE, con = simulate_mssql())

  # as simple symbol and atom
  expect_snapshot(mf %>% filter(x))
  expect_snapshot(mf %>% filter(TRUE))

  # when involved in a (perhaps nested) logical expression
  expect_snapshot(mf %>% filter((!x) | FALSE))

  # in a subquery
  expect_snapshot(mf %>% filter(x) %>% inner_join(mf, by = "x"))
})

# Live database -----------------------------------------------------------

test_that("can copy_to() and compute() with temporary tables (#272)", {
  con <- src_test("mssql")
  df <- tibble(x = 1:3)
  expect_message(
    db <- copy_to(con, df, name = "temp", temporary = TRUE),
    "Created a temporary table",
  )
  expect_equal(db %>% pull(), 1:3)

  expect_message(
    db2 <- db %>% mutate(y = x + 1) %>% compute(),
    "Created a temporary table"
  )
  expect_equal(db2 %>% pull(), 2:4)
})

test_that("bit conversion works for important cases", {
  df <- tibble(x = 1:3, y = 3:1)
  db <- copy_to(src_test("mssql"), df, name = unique_table_name())
  expect_equal(db %>% mutate(z = x == y) %>% pull(), c(FALSE, TRUE, FALSE))
  expect_equal(db %>% filter(x == y) %>% pull(), 2)

  df <- tibble(x = c(TRUE, FALSE, FALSE), y = c(TRUE, FALSE, TRUE))
  db <- copy_to(src_test("mssql"), df, name = unique_table_name())
  expect_equal(db %>% filter(x == 1) %>% pull(), TRUE)
  expect_equal(db %>% mutate(z = TRUE) %>% pull(), c(1, 1, 1))

  # Currently not supported: would need to determine that we have a bit
  # vector in a boolean context, and convert to boolean with x == 1.
  # expect_equal(db %>% mutate(z = x) %>% pull(), c(TRUE, FALSE, FALSE))
  # expect_equal(db %>% mutate(z = !x) %>% pull(), c(FALSE, TRUE, TRUE))
  # expect_equal(db %>% mutate(z = x & y) %>% pull(), c(TRUE, FALSE, FALSE))

})

test_that("as.integer and as.integer64 translations if parsing failures", {
  df <- data.frame(x = c("1.3", "2x"))
  db <- copy_to(src_test("mssql"), df, name = unique_table_name())

  out <- db %>%
    mutate(
      integer = as.integer(x),
      integer64 = as.integer64(x),
      numeric = as.numeric(x),
    ) %>%
    collect()

  expect_identical(out$integer, c(1L, NA))
  expect_identical(out$integer64, bit64::as.integer64(c(1L, NA)))
  expect_identical(out$numeric, c(1.3, NA))
})

test_that("can insert", {
  con <- src_test("mssql")

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y", temporary = TRUE, overwrite = TRUE) %>%
    mutate(c = c + 1)

  expect_equal(
    rows_insert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      conflict = "ignore"
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") %>%
    mutate(c = c + 1)

  expect_equal(
    rows_insert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      conflict = "ignore",
      returning = everything()
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 1:3, b = 11:13, c = -(2:4), d = c("y", "z", "w"))
  y <- local_db_table(con, df_y, "df_y") %>%
    mutate(c = c + 1)

  expect_equal(
    rows_append(
      x, y,
      in_place = TRUE
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1L, b = 11L, c = 1L, d = "a")
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 1:3, b = 11:13, c = -(2:4), d = c("y", "z", "w"))
  y <- local_db_table(con, df_y, "df_y") %>%
    mutate(c = c + 1)

  expect_equal(
    rows_append(
      x, y,
      in_place = TRUE,
      returning = everything()
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") %>%
    mutate(c = c + 1)

  expect_equal(
    rows_update(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore"
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") %>%
    mutate(c = c + 1)

  expect_equal(
    rows_update(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore",
      returning = everything()
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1:2, b = 11:12, c = 1:2, d = c("a", "b"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") %>%
    mutate(c = c + 1)

  expect_equal(
    rows_upsert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1:2, b = 11:12, c = 1:2, d = c("a", "b"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") %>%
    mutate(c = c + 1)

  expect_equal(
    rows_upsert(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      returning = everything()
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
})

test_that("can delete", {
  con <- src_test("mssql")

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L))
  y <- local_db_table(con, df_y, "df_y")

  expect_equal(
    rows_delete(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore"
    ) %>%
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
  con <- src_test("mssql")

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L))
  y <- local_db_table(con, df_y, "df_y")

  expect_equal(
    rows_delete(
      x, y,
      by = c("a", "b"),
      in_place = TRUE,
      unmatched = "ignore",
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(
      a = 2:3,
      b = 12:13,
      c = 2:3,
      d = c("b", "c")
    )
  )
})
