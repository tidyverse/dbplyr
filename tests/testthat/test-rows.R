
# rows_update -------------------------------------------------------------

test_that("early return if no column to update", {
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

test_that("empty `by` works", {
  expect_snapshot(
    rows_update(
      lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      in_place = FALSE
    )
  )
})

test_that("`in_place = FALSE` works", {
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

test_that("`in_place = FALSE` with `returning` works", {
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

test_that("`in_place = TRUE` works", {
  # TODO error for `in_place = TRUE` and lazy frames
  # expect_snapshot(
  #   rows_update(
  #     lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
  #     lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
  #     by = "x",
  #     in_place = TRUE
  #   )
  # )

  df <- memdb_frame(x = 1:3, y = 11:13)
  rows_update(
    df, memdb_frame(x = 2:3, y = 22:23),
    by = "x",
    in_place = TRUE
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = c(11L, 22:23)))
})

test_that("`in_place = TRUE` with `returning` works", {
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


# rows_patch --------------------------------------------------------------

test_that("early return if no column to update", {
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
    rows_update(
      memdb_frame(x = 1:3, y = c(11, 12, NA)),
      memdb_frame(x = 1:3),
      by = "x",
      in_place = TRUE
    ) %>%
      collect(),
    tibble(x = 1:3, y = c(11, 12, NA))
  )
})

test_that("empty `by` works", {
  expect_snapshot(
    rows_patch(
      lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"),
      lazy_frame(x = 1:3, .name = "df_y"),
      in_place = FALSE
    )
  )
})

test_that("`in_place = FALSE` works", {
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

test_that("`in_place = FALSE` with `returning` works", {
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

test_that("`in_place = TRUE` works", {
  # TODO error for `in_place = TRUE` and lazy frames
  # expect_snapshot(
  #   rows_update(
  #     lazy_frame(x = 1:3, y = 11:13, .name = "df_x"),
  #     lazy_frame(x = 2:3, y = 22:23, .name = "df_y"),
  #     by = "x",
  #     in_place = TRUE
  #   )
  # )

  df <- memdb_frame(x = 1:3, y = c(11, 12, NA))
  rows_patch(
    df, memdb_frame(x = 2:3, y = 22:23),
    by = "x",
    in_place = TRUE
  )

  expect_equal(df %>% collect(), tibble(x = 1:3, y = c(11, 12, 23)))
})

test_that("`in_place = TRUE` with `returning` works", {
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
