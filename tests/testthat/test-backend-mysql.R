# function translation ----------------------------------------------------

test_that("custom scalar translated correctly", {
  local_con(simulate_mysql())
  expect_equal(test_translate_sql(as.logical(1L)), sql("IF(1, TRUE, FALSE)"))

  expect_equal(
    test_translate_sql(str_locate("abc", "b")),
    sql("REGEXP_INSTR('abc', 'b')")
  )
  expect_equal(
    test_translate_sql(str_replace_all("abc", "b", "d")),
    sql("REGEXP_REPLACE('abc', 'b', 'd')")
  )
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_mysql())

  expect_equal(
    test_translate_sql(str_flatten(y, ","), window = FALSE),
    sql("GROUP_CONCAT(`y` SEPARATOR ',')")
  )
})

test_that("use CHAR type for as.character", {
  local_con(simulate_mysql())
  expect_equal(test_translate_sql(as.character(x)), sql("CAST(`x` AS CHAR)"))
})

test_that("custom date escaping works as expected", {
  con <- simulate_mysql()

  expect_equal(escape(as.Date("2020-01-01"), con = con), sql("'2020-01-01'"))
  expect_equal(escape(as.Date(NA), con = con), sql("NULL"))

  expect_equal(
    escape(as.POSIXct("2020-01-01", tz = "UTC"), con = con),
    sql("'2020-01-01 00:00:00'")
  )
  expect_equal(escape(as.POSIXct(NA, tz = "UTC"), con = con), sql("NULL"))
})


test_that("custom stringr functions translated correctly", {
  local_con(simulate_mysql())

  expect_equal(test_translate_sql(str_c(x, y)), sql("CONCAT_WS('', `x`, `y`)"))
  expect_equal(test_translate_sql(str_detect(x, y)), sql("`x` REGEXP `y`"))
  expect_equal(test_translate_sql(str_like(x, y)), sql("`x` LIKE `y`"))
  expect_equal(
    test_translate_sql(str_like(x, y, FALSE)),
    sql("`x` LIKE BINARY `y`")
  )
  expect_equal(
    test_translate_sql(str_locate(x, y)),
    sql("REGEXP_INSTR(`x`, `y`)")
  )
  expect_equal(
    test_translate_sql(str_replace_all(x, y, z)),
    sql("REGEXP_REPLACE(`x`, `y`, `z`)")
  )
})

# verbs -------------------------------------------------------------------

test_that("generates custom sql", {
  con_maria <- simulate_mariadb()

  expect_snapshot(sql_table_analyze(con_maria, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_explain(con_maria, sql("SELECT * FROM table")))

  lf <- lazy_frame(x = 1, con = con_maria)
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))
  expect_snapshot(error = TRUE, full_join(lf, lf, by = "x"))

  expect_snapshot(slice_sample(lf, n = 1))

  expect_snapshot(
    copy_inline(con_maria, tibble(x = 1:2, y = letters[1:2])) |> remote_query()
  )

  con_mysql <- simulate_mysql()
  expect_snapshot(
    copy_inline(con_mysql, tibble(x = 1:2, y = letters[1:2])) |> remote_query()
  )
})

test_that("`sql_query_update_from()` is correct", {
  con <- simulate_mysql()
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
        c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"
      )
    )
  )

  expect_snapshot_error(
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

# live database -----------------------------------------------------------

test_that("logicals converted to integer correctly", {
  db <- copy_to_test("MariaDB", data.frame(x = c(TRUE, FALSE, NA)))
  expect_identical(db |> pull(), c(1L, 0L, NA))
})

test_that("can explain", {
  db <- copy_to_test("MariaDB", data.frame(x = 1:3))
  expect_snapshot(db |> mutate(y = x + 1) |> explain())
})

test_that("can overwrite temp tables", {
  con <- src_test("MariaDB")

  df1 <- tibble(x = 1)
  copy_to(con, df1, "test-df", temporary = TRUE)

  df2 <- tibble(x = 2)
  db2 <- copy_to(con, df2, "test-df", temporary = TRUE, overwrite = TRUE)
  expect_equal(collect(db2), df2)
})

test_that("can update", {
  con <- src_test("MariaDB")

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") |>
    mutate(c = c + 1)

  # `RETURNING` in an `UPDATE` clause is not (yet) supported for MariaDB
  # https://jira.mariadb.org/browse/MDEV-5092
  expect_equal(
    rows_update(
      x,
      y,
      by = c("a", "b"),
      unmatched = "ignore",
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

test_that("casts `y` column for local df", {
  con <- src_test("MariaDB")

  DBI::dbExecute(con, "CREATE SCHEMA dbplyr_test_schema")
  withr::defer(DBI::dbExecute(con, "DROP SCHEMA dbplyr_test_schema"))
  df <- tibble(id = 1L, val = 10L, ltext = strrep("a", times = 65535 + 2))
  types <- c(id = "bigint", val = "bigint", ltext = "longtext")
  local_db_table(con, value = df, types = types, temporary = FALSE, "df_x")
  table2 <- DBI::Id(schema = "dbplyr_test_schema", table = "df_x")
  local_db_table(con, value = df, types = types, temporary = FALSE, table2)

  y <- tibble(
    id = "2",
    val = 20,
    ltext = strrep("b", times = 65535 + 2)
  )

  out <- tibble(
    id = bit64::as.integer64(1:2),
    val = bit64::as.integer64(10L, 20L),
    ltext = c(strrep("a", times = 65535 + 2), strrep("b", times = 65535 + 2))
  )
  expect_equal(
    rows_append(
      tbl(con, "df_x"),
      y,
      copy = TRUE,
      in_place = FALSE
    ) |>
      collect(),
    out
  )

  rows_append(
    tbl(con, "df_x"),
    y,
    copy = TRUE,
    in_place = TRUE
  )

  expect_equal(tbl(con, "df_x") |> collect(), out)

  types_expected <- c(id = "bigint(20)", val = "bigint(20)", ltext = "longtext")
  expect_equal(db_col_types(con, table2), types_expected)
  expect_equal(db_col_types(con, in_schema("test", "df_x")), types_expected)
})
