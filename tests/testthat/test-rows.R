
# rows_insert -------------------------------------------------------------

test_that("rows_insert() checks arguments", {
  lf <- lazy_frame(x = 1:3, y = 11:13, .name = "df_x")

  expect_snapshot(error = TRUE, {
    (rows_insert(lf, lf, by = "x", conflict = "error"))
    (rows_insert(lf, lf, by = "x"))
  })

  expect_snapshot(error = TRUE,
    (rows_insert(
      lf,
      lazy_frame(x = 1, y = 2, z = 3),
      by = "x",
      conflict = "ignore"
    ))
  )

  expect_snapshot(error = TRUE,
    (rows_insert(lf, lf, by = "x", conflict = "ignore", returning = everything()))
  )

  df <- memdb_frame(x = 1)
  expect_snapshot(error = TRUE,
    (df %>%
      mutate(x = x + 1) %>%
      rows_insert(df, by = "x", conflict = "ignore", in_place = TRUE))
  )

  expect_snapshot(error = TRUE,
    (df %>%
      rows_insert(df, by = "x", conflict = "ignore", returning = c(y)))
  )
})


test_that("`rows_insert()` works with empty `by`", {
  expect_message(
    rows_insert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      in_place = FALSE,
      conflict = "ignore"
    ),
    regexp = 'Matching, by = "x"'
  )
})

test_that("`rows_insert()` errors for `conflict = 'error'` and `in_place = FALSE`", {
  lf <- lazy_frame(x = 1:3, y = 11:13, .name = "df_x")
  expect_snapshot(
    (expect_error(
      rows_insert(
        lf, lf,
        by = "x",
        conflict = "error",
        in_place = FALSE
      )
    ))
  )
})

test_that("`rows_insert()` works with `in_place = FALSE`", {
  expect_snapshot(
    rows_insert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 3:4, y = 23:24, .name = "df_y"),
      by = "x",
      conflict = "ignore",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  expect_equal(
    rows_insert(
      df, memdb_frame(x = 3:4, y = 23:24),
      by = "x",
      conflict = "ignore",
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = 1:4, y = c(11:13, 24))
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = 11:13))
})

test_that("`rows_insert()` works with `in_place = FALSE` and `returning`", {
  expect_equal(
    rows_insert(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 3:4, y = 23:24),
      by = "x",
      conflict = "ignore",
      in_place = FALSE,
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 4, y = 24)
  )

  # all `x` columns are present
  expect_equal(
    rows_insert(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 3:4),
      by = "x",
      conflict = "ignore",
      in_place = FALSE,
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 4, y = NA)
  )
})

test_that("`rows_insert()` works with `in_place = TRUE`", {
  expect_snapshot(error = TRUE,
    (rows_insert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = TRUE
    ))
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  rows_insert(
    df, memdb_frame(x = 3:4, y = 23:24),
    by = "x",
    conflict = "ignore",
    in_place = TRUE
  )
  expect_equal(df %>% collect(), tibble(x = 1:4, y = c(11:13, 24)))
})

test_that("`rows_insert()` with `in_place = TRUE` and `returning`", {
  skip_if_not_installed("RSQLite", "2.2.8")

  df <- memdb_frame(x = 1:3, y = 11:13)
  df_inserted <- rows_insert(
    df, memdb_frame(x = 3:4, y = 23:24),
    by = "x",
    conflict = "ignore",
    in_place = TRUE,
    returning = everything()
  )

  expect_equal(get_returned_rows(df_inserted), tibble(x = 4L, y = 24L))

  expect_equal(df_inserted %>% collect(), tibble(x = 1:4, y = c(11:13, 24L)))
})

test_that("rows_get_or_execute() gives error context", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "mtcars", tibble(x = 1, y = 1), overwrite = TRUE, temporary = TRUE)
  DBI::dbExecute(con, "CREATE UNIQUE INDEX `mtcars_x` ON `mtcars` (`x`)")

  expect_error(
    rows_append(
      tbl(con, "mtcars"),
      tibble(x = 1),
      copy = TRUE,
      in_place = TRUE
    )
  )

  expect_error(
    rows_append(
      tbl(con, "mtcars"),
      tibble(x = 1),
      copy = TRUE,
      in_place = TRUE,
      returning = x
    )
  )
})

test_that("`sql_query_insert()` works", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_dbi(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(error = TRUE,
    (sql_query_insert(
      con = simulate_dbi(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      conflict = "error",
      returning_cols = c("a", b2 = "b")
    ))
  )

  expect_snapshot(
    sql_query_insert(
      con = simulate_dbi(),
      x_name = ident("df_x"),
      y = df_y,
      by = c("a", "b"),
      conflict = "ignore",
      returning_cols = c("a", b2 = "b")
    )
  )
})

# rows_append -------------------------------------------------------------

test_that("rows_append() checks arguments", {
  lf <- lazy_frame(x = 1:3, y = 11:13, .name = "df_x")
  expect_snapshot(error = TRUE, {
    (lf %>% rows_append(df, by = "x"))
    (lf %>% rows_append(df, conflict = "error"))
  })
})


test_that("`rows_append()` works with `in_place = FALSE`", {
  expect_snapshot(
    rows_append(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 3:4, y = 23:24, .name = "df_y"),
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  expect_equal(
    rows_append(
      df, memdb_frame(x = 3:4, y = 23:24),
      in_place = FALSE
    ) %>%
      collect(),
    tibble(x = c(1:3, 3:4), y = c(11:13, 23:24))
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = 11:13))
})

test_that("`rows_append()` works with `in_place = FALSE` and `returning`", {
  expect_equal(
    rows_append(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 3:4, y = 23:24),
      in_place = FALSE,
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 3:4, y = 23:24)
  )

  # all `x` columns are present
  expect_equal(
    rows_append(
      memdb_frame(x = 1:3, y = 11:13),
      memdb_frame(x = 3:4),
      in_place = FALSE,
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 3:4, y = NA)
  )
})

test_that("`rows_append()` works with `in_place = TRUE`", {
  expect_snapshot(error = TRUE,
    (rows_append(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      in_place = TRUE
    ))
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  rows_append(
    df, memdb_frame(x = 3:4, y = 23:24),
    in_place = TRUE
  )
  expect_equal(df %>% collect(), tibble(x = c(1:3, 3:4), y = c(11:13, 23:24)))
})

test_that("`rows_append()` with `in_place = TRUE` and `returning`", {
  skip_if_not_installed("RSQLite", "2.2.8")

  df <- memdb_frame(x = 1:3, y = 11:13)
  df_inserted <- rows_append(
    df, memdb_frame(x = 3:4, y = 23:24),
    in_place = TRUE,
    returning = everything()
  )

  expect_equal(get_returned_rows(df_inserted), tibble(x = 3:4, y = 23:24))

  expect_equal(df_inserted %>% collect(), tibble(x = c(1:3, 3:4), y = c(11:13, 23:24)))
})

test_that("`sql_query_append()` works", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_dbi(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_append(
      con = simulate_dbi(),
      x_name = ident("df_x"),
      y = df_y,
      returning_cols = c("a", b2 = "b")
    )
  )
})

# rows_update -------------------------------------------------------------

test_that("arguments are checked", {
  lf <- lazy_frame(x = 1:3, y = 11:13, .name = "df_x")

  expect_snapshot(error = TRUE, (rows_update(lf, lf, by = 1, unmatched = "ignore")))

  expect_snapshot(error = TRUE, (rows_update(lf, lf, by = c(y = "x"), unmatched = "ignore")))

  expect_snapshot(error = TRUE, (rows_update(lf, lf, by = "z", unmatched = "ignore")))

  expect_snapshot(error = TRUE, {
    (rows_update(lf, lf, by = "x", unmatched = "error"))
    (rows_update(lf, lf, by = "x"))
  })

  expect_snapshot(error = TRUE,
    (rows_update(
      lf,
      lazy_frame(x = 1, y = 2, z = 3),
      by = "x",
      unmatched = "ignore"
    ))
  )

  expect_snapshot(error = TRUE,
    (rows_update(lf, lf, by = "x", unmatched = "ignore", returning = everything()))
  )

  df <- memdb_frame(x = 1)
  expect_snapshot(error = TRUE,
    (df %>%
      mutate(x = x + 1) %>%
      rows_update(df, by = "x", unmatched = "ignore", in_place = TRUE))
  )
})

test_that("`rows_update()` returns early if no column to update", {
  lf <- lazy_frame(x = 1:3, y = 11:13, .name = "df_x")
  expect_equal(
    rows_update(
      lf,
      lazy_frame(x = 1:3, .name = "df_y"),
      by = "x",
      unmatched = "ignore",
      in_place = FALSE
    ),
    lf
  )

  db <- memdb_frame(x = 1:3, y = 11:13)
  expect_equal(
    rows_update(
      db,
      memdb_frame(x = 1:3),
      by = "x",
      unmatched = "ignore",
      in_place = FALSE
    ) %>%
      collect(),
    collect(db)
  )

  expect_equal(
    rows_update(
      db,
      memdb_frame(x = 1:3),
      by = "x",
      unmatched = "ignore",
      in_place = TRUE
    ) %>%
      collect(),
    collect(db)
  )
})

test_that("`rows_update()` works with empty `by`", {
  expect_message(
    rows_update(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      unmatched = "ignore",
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
      unmatched = "ignore",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  expect_equal(
    rows_update(
      df, memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      unmatched = "ignore",
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
      unmatched = "ignore",
      in_place = FALSE,
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 2:3, y = 22:23)
  )
})

test_that("`rows_update()` works with `in_place = TRUE`", {
  expect_snapshot(error = TRUE,
    (rows_update(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      unmatched = "ignore",
      in_place = TRUE
    ))
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  rows_update(
    df, memdb_frame(x = 2:3, y = 22:23),
    by = "x",
    unmatched = "ignore",
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
    unmatched = "ignore",
    in_place = TRUE,
    returning = everything()
  )

  expect_equal(get_returned_rows(df_updated), tibble(x = 2:3, y = 22:23))

  expect_equal(df_updated %>% collect(), tibble(x = 1:3, y = c(11L, 22:23)))
})

test_that("`sql_query_update_from()` works", {
  df_y <- lazy_frame(
    a = 2:3, b = c(12L, 13L), c = -(2:3), d = c("y", "z"),
    con = simulate_dbi(),
    .name = "df_y"
  ) %>%
    mutate(c = c + 1)

  expect_snapshot(
    sql_query_update_from(
      con = simulate_dbi(),
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
      unmatched = "ignore",
      in_place = FALSE
    )
  )

  expect_equal(
    rows_patch(
      memdb_frame(x = 1:3, y = c(11, 12, NA)),
      memdb_frame(x = 1:3),
      by = "x",
      unmatched = "ignore",
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
      unmatched = "ignore",
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
      unmatched = "ignore",
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
      unmatched = "ignore",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  expect_equal(
    rows_patch(
      df, memdb_frame(x = 2:3, y = 22:23),
      by = "x",
      unmatched = "ignore",
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
      unmatched = "ignore",
      in_place = FALSE,
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 2:3, y = c(12, 23))
  )
})

test_that("`rows_patch()` works with `in_place = TRUE`", {
  expect_snapshot(error = TRUE,
    (rows_patch(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      unmatched = "ignore",
      in_place = TRUE
    ))
  )

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  rows_patch(
    df, memdb_frame(x = 2:3, y = 22:23),
    by = "x",
    unmatched = "ignore",
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
    unmatched = "ignore",
    in_place = TRUE,
    returning = everything()
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
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 2:4, y = 22:24)
  )
})

test_that("`rows_upsert()` works with `in_place = TRUE`", {
  expect_snapshot(error = TRUE,
    (rows_upsert(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
      by = "x",
      in_place = TRUE
    ))
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
    returning = everything()
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
      unmatched = "ignore",
      in_place = FALSE
    ),
    regexp = 'Matching, by = "x"'
  )
})

test_that("`rows_delete()` ignores extra `y` columns", {
  expect_message(
    rows_delete(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 1:3, y = 11, .name = "df_y"),
      by = "x",
      unmatched = "ignore",
      in_place = FALSE
    ),
    regexp = 'Ignoring extra `y` columns: `y`'
  )
})

test_that("`rows_delete()` works with `in_place = FALSE`", {
  expect_snapshot(
    rows_delete(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 2:3, .name = "df_y"),
      by = "x",
      unmatched = "ignore",
      in_place = FALSE
    )
  )

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  expect_equal(
    rows_delete(
      df, memdb_frame(x = 2:3),
      by = "x",
      unmatched = "ignore",
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
      memdb_frame(x = 2:3),
      by = "x",
      unmatched = "ignore",
      in_place = FALSE,
      returning = everything()
    ) %>%
      get_returned_rows(),
    tibble(x = 2:3, y = 12:13)
  )
})

test_that("`rows_delete()` works with `in_place = TRUE`", {
  expect_snapshot(error = TRUE,
    (rows_delete(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 2:3, .name = "df_y"),
      by = "x",
      unmatched = "ignore",
      in_place = TRUE
    ))
  )

  df <- memdb_frame(x = 1:3, y = 11:13)
  rows_delete(
    df, memdb_frame(x = 2:3),
    by = "x",
    unmatched = "ignore",
    in_place = TRUE
  )

  expect_equal(df %>% collect(), tibble(x = 1L, y = 11L))
})

test_that("`rows_delete()` works with `in_place = TRUE` and `returning`", {
  skip_if_not_installed("RSQLite", "2.2.8")

  df <- memdb_frame(x = 1:3, y = 11:13)
  df_deleted <- rows_delete(
    df, memdb_frame(x = 2:4),
    by = "x",
    unmatched = "ignore",
    in_place = TRUE,
    returning = everything()
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
  expect_snapshot(error = TRUE, (get_returned_rows(df)))

  df2 <- set_returned_rows(df, mtcars)
  expect_true(has_returned_rows(df2))
  expect_equal(get_returned_rows(df2), as_tibble(mtcars))
})
