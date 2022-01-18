
# rows_update -------------------------------------------------------------

test_that("arguments are checked", {
  lf <- lazy_frame(x = 1:3, y = 11:13, .name = "df_x")

  expect_snapshot_error(rows_update(lf, lf, by = 1))

  expect_snapshot_error(rows_update(lf, lf, by = c(y = "x")))

  expect_snapshot_error(rows_update(lf, lf, by = "z"))

  expect_snapshot_error(
    rows_update(
      lf,
      lazy_frame(x = 1, y = 2, z = 3),
      by = "x"
    )
  )

  expect_snapshot_error(
    rows_update(lf, lf, by = "x", returning = quote(everything()))
  )

  df <- memdb_frame(x = 1)
  expect_snapshot_error(
    df %>%
      mutate(x = x + 1) %>%
      rows_update(df, by = "x", in_place = TRUE)
  )
})

test_that("`rows_update()` returns early if no column to update", {
  expect_snapshot(
    rows_update(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      by = "x",
      in_place = FALSE
    )
  )

  expect_equal(
    rows_update(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 1:3),
      by = "x",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1:3, y = 11:13)
  )

  expect_equal(
    rows_update(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 1:3),
      by = "x",
      in_place = TRUE
    ) %>%
      collect(),
    tibble(x = 1:3, y = 11:13)
  )
})

test_that("`rows_update()` works with empty `by`", {
  expect_message(
    rows_update(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      in_place = FALSE
    ),
    regexp = 'Matching, by = "x"'
  )
})

test_that("`rows_update()` works with `in_place = FALSE`", {
  expect_snapshot(
    rows_update(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  expect_equal(
    rows_update(
      df, memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1:3, y = c(11L, 22:23))
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = 11:13))
})

test_that("`rows_update()` works with `in_place = FALSE` and `returning`", {
  expect_equal(
    rows_update(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      in_place = FALSE,
      returning = quote(everything())
    ) %>%
      get_returned_rows(),
    tibble(x = 2:3, y = 22:23)
  )
})

test_that("`rows_update()` works with `in_place = TRUE`", {
  expect_snapshot_error(
    rows_update(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = TRUE
    )
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  rows_update(
    df, memdb_frame(x = 2:3, y = 22:23),
    by = "x",
    in_place = TRUE
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = c(11L, 22:23)))
})

test_that("`rows_update()` with `in_place = TRUE` and `returning`", {
  skip_if_not_installed("RSQLite", "2.2.8")

  df <- memdb_frame(x = 1:3, y = 11:13)
  df_updated <- rows_update(
    df, memdb_frame(x = 2:4, y = 22:24),
    by = "x",
    in_place = TRUE,
    returning = quote(everything())
  )

  expect_equal(get_returned_rows(df_updated), tibble(x = 2:3, y = 22:23))

  expect_equal(df_updated %>% collect(), tibble(x = 1:3, y = c(11L, 22:23)))
})

test_that("`sql_query_update_from()` works", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_mssql(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_update_from(
      con = simulate_mssql(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      update_values = sql(
        c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"
      ),
      returning_cols = c("a", b2 = "b")
    )
  )
})

# rows_patch --------------------------------------------------------------

test_that("`rows_patch()` returns early if no column to update", {
  expect_snapshot(
    rows_patch(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      by = "x",
      in_place = FALSE
    )
  )

  expect_equal(
    rows_patch(
      memdb_frame(x = 1:3, y = c(11, 12, NA)),
      memdb_frame(x = 1:3),
      by = "x",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1:3, y = c(11, 12, NA))
  )

  expect_equal(
    rows_patch(
      memdb_frame(x = 1:3, y = c(11, 12, NA)),
      memdb_frame(x = 1:3),
      by = "x",
      in_place = TRUE
    ) %>%
      collect(),
    tibble(x = 1:3, y = c(11, 12, NA))
  )
})

test_that("`rows_patch()` works with empty `by`", {
  expect_message(
    rows_patch(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      in_place = FALSE
    ),
    regexp = 'Matching, by = "x"'
  )
})

test_that("`rows_patch()` works with `in_place = FALSE`", {
  expect_snapshot(
    rows_patch(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  expect_equal(
    rows_patch(
      df, memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1:3, y = c(11, 12, 23))
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = c(11, 12, NA)))
})

test_that("`rows_patch()` works with `in_place = FALSE` and `returning`", {
  expect_equal(
    rows_patch(
      memdb_frame(x = 1:3, y = c(11, 12, NA)),
      memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      in_place = FALSE,
      returning = quote(everything())
    ) %>%
      get_returned_rows(),
    tibble(x = 2:3, y = c(12, 23))
  )
})

test_that("`rows_patch()` works with `in_place = TRUE`", {
  expect_snapshot_error(
    rows_patch(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = TRUE
    )
  )

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  rows_patch(
    df, memdb_frame(x = 2:3, y = 22:23),
    by = "x",
    in_place = TRUE
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = c(11, 12, 23)))
})

test_that("`rows_patch()` works with `in_place = TRUE` and `returning`", {
  skip_if_not_installed("RSQLite", "2.2.8")

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  df_patched <- rows_patch(
    df, memdb_frame(x = 2:4, y = 22:24),
    by = "x",
    in_place = TRUE,
    returning = quote(everything())
  )

  expect_equal(get_returned_rows(df_patched), tibble(x = 2:3, y = c(12, 23)))

  expect_equal(df_patched %>% collect(), tibble(x = 1:3, y = c(11, 12, 23)))
})


# rows_upsert -------------------------------------------------------------

test_that("`rows_upsert()` returns early if no column to update", {
  expect_snapshot(
    rows_upsert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      by = "x",
      in_place = FALSE
    )
  )

  expect_equal(
    rows_upsert(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 2:4),
      by = "x",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1:4, y = c(11:13, NA))
  )

  expect_equal(
    rows_upsert(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 2:4),
      by = "x",
      in_place = TRUE
    ) %>%
      collect(),
    tibble(x = 1:3, y = 11:13)
  )
})

test_that("`rows_upsert()` works with empty `by`", {
  expect_message(
    rows_upsert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      in_place = FALSE
    ),
    regexp = 'Matching, by = "x"'
  )
})

test_that("`rows_upsert()` works with `in_place = FALSE`", {
  expect_snapshot(
    rows_upsert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  expect_equal(
    rows_upsert(
      df, memdb_frame(x = 2:4, y = 22:24),
      by = "x",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1:4, y = c(11L, 22:24))
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = 11:13))
})

test_that("`rows_upsert()` works with `in_place = FALSE` and `returning`", {
  expect_equal(
    rows_upsert(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 2:4, y = 22:24),
      by = "x",
      in_place = FALSE,
      returning = quote(everything())
    ) %>%
      get_returned_rows(),
    tibble(x = 2:4, y = 22:24)
  )
})

test_that("`rows_upsert()` works with `in_place = TRUE`", {
  expect_snapshot_error(
    rows_upsert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = TRUE
    )
  )

  skip_if_not_installed("RSQLite", "2.2.8")
  con <- src_memdb()
  x <- copy_to(con, tibble(x = 1:3, y = 11:13), name = "df_x", overwrite = TRUE, unique_indexes = "x")
  y <- copy_to(con, tibble(x = 2:4, y = 22:24), name = "df_y", overwrite = TRUE)

  rows_upsert(
    x, y,
    by = "x",
    in_place = TRUE
  )

  expect_equal(x %>% collect(), tibble(x = 1:4, y = c(11L, 22:24)))
})

test_that("`rows_upsert()` works with `in_place = TRUE` and `returning`", {
  skip_if_not_installed("RSQLite", "2.2.8")

  con <- src_memdb()
  x <- copy_to(con, tibble(x = 1:3, y = 11:13), name = "df_x", overwrite = TRUE, unique_indexes = "x")
  y <- copy_to(con, tibble(x = 2:4, y = 22:24), name = "df_y", overwrite = TRUE)
  df_upserted <- rows_upsert(
    x, y,
    by = "x",
    in_place = TRUE,
    returning = quote(everything())
  )

  expect_equal(get_returned_rows(df_upserted), tibble(x = 4, y = 24))

  expect_equal(df_upserted %>% collect(), tibble(x = 1:4, y = c(11L, 22:24)))
})

test_that("`sql_query_upsert()` is correct", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_dbi(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_upsert(
      con = simulate_dbi(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      update_cols = c("c", "d"),
      returning_cols = c("a", b2 = "b")
    )
  )
})

# rows_delete -------------------------------------------------------------

test_that("`rows_delete()` works with empty `by`", {
  expect_message(
    rows_delete(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      in_place = FALSE
    ),
    regexp = 'Matching, by = "x"'
  )
})

test_that("`rows_delete()` works with `in_place = FALSE`", {
  expect_snapshot(
    rows_delete(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  expect_equal(
    rows_delete(
      df, memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1, y = 11)
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = c(11, 12, NA)))
})

test_that("`rows_delete()` works with `in_place = FALSE` with `returning`", {
  expect_equal(
    rows_delete(
      memdb_frame(x = 1:3, y = c(11, 12, 13)),
      memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      in_place = FALSE,
      returning = quote(everything())
    ) %>%
      get_returned_rows(),
    tibble(x = 2:3, y = 12:13)
  )
})

test_that("`rows_delete()` works with `in_place = TRUE`", {
  expect_snapshot_error(
    rows_delete(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = TRUE
    )
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  rows_delete(
    df, memdb_frame(x = 2:3, y = 22:23),
    by = "x",
    in_place = TRUE
  )

  expect_equal(df %>% collect(), tibble(x = 1L, y = 11L))
})

test_that("`rows_delete()` works with `in_place = TRUE` and `returning`", {
  skip_if_not_installed("RSQLite", "2.2.8")

  df <- memdb_frame(x = 1:3, y = 11:13)
  df_deleted <- rows_delete(
    df, memdb_frame(x = 2:4, y = 22:24),
    by = "x",
    in_place = TRUE,
    returning = quote(everything())
  )

  expect_equal(get_returned_rows(df_deleted), tibble(x = 2:3, y = 12:13))

  expect_equal(df_deleted %>% collect(), tibble(x = 1L, y = 11L))
})

test_that("`sql_query_delete()` is correct", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_dbi(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_delete(
      con = simulate_dbi(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      returning_cols = c("a", b2 = "b")
    )
  )
})

# returned_rows -----------------------------------------------------------

test_that("`get_returned_rows()` works", {
  df <- tibble(x = 1)
  expect_false(has_returned_rows(df))
  expect_snapshot_error(get_returned_rows(df))

  df2 <- set_returned_rows(df, mtcars)
  expect_true(has_returned_rows(df2))
  expect_equal(get_returned_rows(df2), as_tibble(mtcars))
})
