test_that("simulate_mysql() and simulate_mariadb() still work", {
  expect_translation(simulate_mysql(), x + 1, "`x` + 1.0")
  expect_translation(simulate_mariadb(), x + 1, "`x` + 1.0")
})

# function translation ----------------------------------------------------

test_that("custom scalar translated correctly", {
  con <- dialect_mysql()
  expect_translation(con, as.logical(1L), "IF(1, TRUE, FALSE)")

  expect_translation(con, str_locate("abc", "b"), "REGEXP_INSTR('abc', 'b')")
  expect_translation(
    con,
    str_replace_all("abc", "b", "d"),
    "REGEXP_REPLACE('abc', 'b', 'd')"
  )
})

test_that("custom aggregators translated correctly", {
  con <- dialect_mysql()

  expect_translation(
    con,
    str_flatten(y, ","),
    "GROUP_CONCAT(`y` SEPARATOR ',')",
    window = FALSE
  )
})

test_that("use CHAR type for as.character", {
  con <- dialect_mysql()
  expect_translation(con, as.character(x), "CAST(`x` AS CHAR)")
})

test_that("custom date escaping works as expected", {
  con <- dialect_mysql()

  expect_equal(escape(as.Date("2020-01-01"), con = con), sql("'2020-01-01'"))
  expect_equal(escape(as.Date(NA), con = con), sql("NULL"))

  expect_equal(
    escape(as.POSIXct("2020-01-01", tz = "UTC"), con = con),
    sql("'2020-01-01 00:00:00'")
  )
  expect_equal(escape(as.POSIXct(NA, tz = "UTC"), con = con), sql("NULL"))
})


test_that("custom stringr functions translated correctly", {
  con <- dialect_mysql()

  expect_translation(con, str_c(x, y), "CONCAT_WS('', `x`, `y`)")
  expect_translation(con, str_detect(x, y), "`x` REGEXP `y`")
  expect_translation(con, str_like(x, y), "`x` LIKE BINARY `y`")
  expect_translation(con, str_ilike(x, y), "`x` LIKE `y`")

  expect_translation(con, str_locate(x, y), "REGEXP_INSTR(`x`, `y`)")
  expect_translation(
    con,
    str_replace_all(x, y, z),
    "REGEXP_REPLACE(`x`, `y`, `z`)"
  )
})

# verbs -------------------------------------------------------------------

test_that("generates custom sql", {
  con_maria <- dialect_mariadb()

  expect_snapshot(sql_table_analyze(con_maria, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_explain(con_maria, sql("SELECT * FROM table")))

  lf <- lazy_frame(x = 1, con = con_maria)
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))
  expect_snapshot(error = TRUE, full_join(lf, lf, by = "x"))

  expect_snapshot(slice_sample(lf, n = 1))

  expect_snapshot(
    copy_inline(con_maria, tibble(x = 1:2, y = letters[1:2])) |> remote_query()
  )

  con_mysql <- dialect_mysql()
  expect_snapshot(
    copy_inline(con_mysql, tibble(x = 1:2, y = letters[1:2])) |> remote_query()
  )
})

test_that("`sql_query_update_from()` is correct", {
  con <- dialect_mysql()
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
  db <- copy_to(
    test_mariadb(),
    data.frame(x = c(TRUE, FALSE, NA)),
    name = "test",
    overwrite = TRUE
  )
  expect_identical(db |> pull(), c(1L, 0L, NA))
})

test_that("can explain", {
  db <- copy_to(
    test_mariadb(),
    data.frame(x = 1:3),
    name = "test",
    overwrite = TRUE
  )
  expect_snapshot(db |> mutate(y = x + 1) |> explain())
})

test_that("can overwrite temp tables", {
  con <- test_mariadb()

  df1 <- tibble(x = 1)
  copy_to(con, df1, "test-df", temporary = TRUE)
  withr::defer(DBI::dbRemoveTable(con, "test-df"))

  df2 <- tibble(x = 2)
  db2 <- copy_to(con, df2, "test-df", temporary = TRUE, overwrite = TRUE)
  expect_equal(collect(db2), df2)
})

test_that("can update", {
  con <- test_mariadb()

  df_x <- tibble(a = 1:3, b = 11:13, c = 1:3, d = c("a", "b", "c"))
  x <- local_db_table(con, df_x, "df_x")
  df_y <- tibble(a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"))
  y <- local_db_table(con, df_y, "df_y") |> mutate(c = c + 1)

  # `RETURNING` in an `UPDATE` clause is not (yet) supported for MariaDB
  # https://jira.mariadb.org/browse/MDEV-5092
  rows_update(x, y, by = c("a", "b"), unmatched = "ignore", in_place = TRUE)
  expect_equal(
    collect(x),
    tibble(
      a = 1:3,
      b = 11:13,
      c = c(1L, -1L, -2L),
      d = c("a", "y", "z")
    )
  )
})

test_that("casts integer, bigint, and longtext columns", {
  con <- test_mariadb()

  df1 <- tibble(id = 1L, val = 10L, ltext = strrep("a", times = 65535 + 2))
  df2 <- tibble(id = "2", val = 20, ltext = strrep("b", times = 65535 + 2))
  out <- tibble(
    # output is a bigint, since that's all MariaDB has
    id = bit64::as.integer64(1:2),
    val = bit64::as.integer64(c(10L, 20L)),
    ltext = c(df1$ltext, df2$ltext)
  )

  types <- c(id = "integer", val = "bigint", ltext = "longtext")

  db1 <- local_db_table(con, df1, "df1", types = types, temporary = FALSE)
  db2 <- local_db_table(con, df2, "df2", types = types, temporary = FALSE)

  # Via query
  expect_equal(collect(rows_append(db1, db2, copy = TRUE)), out)

  # Via modification
  rows_append(db1, db2, copy = TRUE, in_place = TRUE)
  expect_equal(collect(db1), out)

  types_expected <- c(id = "int(11)", val = "bigint(20)", ltext = "longtext")
  expect_equal(db_col_types(con, "df2"), types_expected)
})
