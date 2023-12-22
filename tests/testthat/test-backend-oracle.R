test_that("custom scalar functions translated correctly", {
  local_con(simulate_oracle())

  expect_equal(test_translate_sql(as.character(x)), sql("CAST(`x` AS VARCHAR2(255))"))
  expect_equal(test_translate_sql(as.integer64(x)), sql("CAST(`x` AS NUMBER(19))"))
  expect_equal(test_translate_sql(as.double(x)),    sql("CAST(`x` AS NUMBER)"))
  expect_equal(test_translate_sql(as.Date(x)),      sql("DATE `x`"))
  expect_equal(test_translate_sql(as.Date("2023-01-01")), sql("DATE '2023-01-01'"))
})

test_that("paste and paste0 translate correctly", {
  local_con(simulate_oracle())

  expect_equal(test_translate_sql(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(test_translate_sql(paste0(x, y)), sql("`x` || `y`"))
  expect_equal(test_translate_sql(str_c(x, y)), sql("`x` || `y`"))
})


test_that("string functions translate correctly", {
  lf <- lazy_frame(x = "yy", con = simulate_oracle())

  expect_snapshot(lf |> mutate(x = str_replace(x,"y","z")))
  expect_snapshot(lf |> mutate(x = str_replace_all(x,"y","z")))
})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, con = simulate_oracle())
  expect_snapshot(mf %>% head())
})

test_that("`sql_query_upsert()` is correct", {
  con <- simulate_oracle()
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
      returning_cols = c("a", b2 = "b"),
      method = "merge"
    )
  )
})

test_that("generates custom sql", {
  con <- simulate_oracle()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_explain(con, sql("SELECT * FROM foo")))

  lf <- lazy_frame(x = 1, con = con)
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))

  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl")))
  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"), temporary = FALSE))

  expect_snapshot(slice_sample(lf, n = 1))
})

test_that("copy_inline uses UNION ALL", {
  con <- simulate_oracle()
  y <- tibble::tibble(id = 1L, arr = "{1,2,3}")

  types <- c(id = "bigint", arr = "integer[]")
  expect_snapshot({
    copy_inline(con, y %>% slice(0)) %>% remote_query()
    copy_inline(con, y) %>% remote_query()

    # with `types`
    copy_inline(con, y %>% slice(0), types = types) %>% remote_query()
    copy_inline(con, y, types = types) %>% remote_query()
  })
})
