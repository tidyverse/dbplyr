test_that("simulate_hana() still works", {
  expect_translation(simulate_hana(), x + 1, '"x" + 1.0')
})

test_that("temporary tables get # prefix", {
  con <- dialect_hana()
  expect_snapshot(
    out <- sql_table_temporary(con, table_path("foo"), temporary = TRUE)
  )
  expect_equal(out, list(table = table_path('"#foo"'), temporary = TRUE))
})

test_that("temporary tables use CREATE LOCAL TEMPORARY COLUMN", {
  con <- dialect_hana()

  expect_snapshot(
    sql_query_save(con, sql("SELECT * FROM foo"), table_path("#tmp"))
  )
  expect_snapshot(
    sql_query_save(
      con,
      sql("SELECT * FROM foo"),
      table_path("tbl"),
      temporary = FALSE
    )
  )

  fields <- c(x = "INTEGER", y = "VARCHAR(10)")
  expect_snapshot(sql_table_create(con, table_path("#tmp"), fields))
})

test_that("custom string translations", {
  con <- dialect_hana()

  expect_snapshot(translate_sql(paste0("a", "b"), con = con))
  expect_snapshot(translate_sql(paste("a", "b"), con = con))

  expect_snapshot(translate_sql(substr(x, 2, 4), con = con))
  expect_snapshot(translate_sql(substring(x, 2, 4), con = con))
  expect_snapshot(translate_sql(str_sub(x, 2, -2), con = con))
})

test_that("copy_inline uses UNION ALL", {
  con <- dialect_hana()
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
