test_that("custom scalar functions translated correctly", {
  con <- dialect_oracle()

  expect_translation(
    con,
    as.character(x),
    'CAST("x" AS VARCHAR2(255))'
  )
  expect_translation(
    con,
    as.integer64(x),
    'CAST("x" AS NUMBER(19))'
  )
  expect_translation(con, as.double(x), 'CAST("x" AS NUMBER)')
  expect_translation(con, as.Date(x), 'DATE "x"')
  expect_translation(
    con,
    as.Date("2023-01-01"),
    "DATE '2023-01-01'"
  )
})

test_that("paste and paste0 translate correctly", {
  con <- dialect_oracle()

  expect_translation(con, paste(x, y), '"x" || \' \' || "y"')
  expect_translation(con, paste0(x, y), '"x" || "y"')
  expect_translation(con, str_c(x, y), '"x" || "y"')
})


test_that("string functions translate correctly", {
  con <- dialect_oracle()

  expect_snapshot({
    translate_sql(str_replace(col, "pattern", "replacement"), con = con)
    translate_sql(str_replace_all(col, "pattern", "replacement"), con = con)
  })
})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, con = dialect_oracle())
  expect_snapshot(mf |> head())
})

test_that("`sql_query_upsert()` is correct", {
  con <- dialect_oracle()
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
      returning_cols = c("a", b2 = "b"),
      method = "merge"
    )
  )
})

test_that("db_table_temporary adds ORA$PTT_ prefix", {
  con <- dialect_oracle()

  # Adds prefix (with message) for temporary tables
  expect_snapshot(
    result <- sql_table_temporary(con, table_path("tbl"), temporary = TRUE)
  )
  expect_equal(as.character(result$table), "ORA$PTT_tbl")
  expect_true(result$temporary)

  # Doesn't double-prefix if already has ORA$PTT_
  result <- sql_table_temporary(
    con,
    table_path("ORA$PTT_tbl"),
    temporary = TRUE
  )
  expect_equal(as.character(result$table), "ORA$PTT_tbl")
  expect_true(result$temporary)

  # Returns table unchanged for non-temporary
  result <- sql_table_temporary(con, table_path("tbl"), temporary = FALSE)
  expect_equal(as.character(result$table), "tbl")
  expect_false(result$temporary)
})

test_that("generates custom sql", {
  con <- dialect_oracle()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
  # Can't analyze private temporary tables
  expect_null(sql_table_analyze(con, table_path("ORA$PTT_tbl")))

  expect_snapshot(sql_query_explain(con, sql("SELECT * FROM foo")))

  lf <- lazy_frame(x = 1, con = con)
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))

  # With ORA$PTT_ prefix -> creates PRIVATE TEMPORARY TABLE
  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), "ORA$PTT_tbl"))
  # Without ORA$PTT_ prefix -> creates regular TABLE
  expect_snapshot(sql_query_save(
    con,
    sql("SELECT * FROM foo"),
    "tbl",
    temporary = FALSE
  ))

  expect_snapshot(slice_sample(lf, n = 1))
})

test_that("oracle_sql_table_create generates correct SQL", {
  con <- dialect_oracle()

  expect_snapshot({
    # Temporary table (has ORA$PTT_ prefix)
    oracle_sql_table_create(
      con,
      table_path("ORA$PTT_test"),
      c(x = "INTEGER", y = "TEXT")
    )

    # Regular table
    oracle_sql_table_create(
      con,
      table_path("test"),
      c(x = "INTEGER", y = "TEXT")
    )
  })
})

test_that("copy_inline uses UNION ALL", {
  con <- dialect_oracle()
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
  con <- dialect_oracle()
  expect_translation(
    con,
    add_years(x, 1),
    '("x" + NUMTODSINTERVAL(1.0 * 365.25, \'day\'))'
  )
  expect_translation(
    con,
    add_days(x, 1),
    '("x" + NUMTODSINTERVAL(1.0, \'day\'))'
  )
  expect_error(
    translate_sql(add_days(x, 1, "dots", "must", "be empty"), con = con),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("difftime is translated correctly", {
  con <- dialect_oracle()
  expect_translation(
    con,
    difftime(start_date, end_date, units = "days"),
    'CEIL(CAST("end_date" AS DATE) - CAST("start_date" AS DATE))'
  )
  expect_translation(
    con,
    difftime(start_date, end_date),
    'CEIL(CAST("end_date" AS DATE) - CAST("start_date" AS DATE))'
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
