test_that("expand completes all values", {
  skip_if_not_installed("tidyr")
  expand <- tidyr::expand

  expect_equal(
    memdb_frame(x = 1:2, y = 1:2) %>% expand(x, y) %>% collect(),
    tibble(x = c(1, 1, 2, 2), y = c(1, 2, 1, 2))
  )

  expect_snapshot(lazy_frame(x = 1, y = 1) %>% expand(x, y))
})

test_that("nesting doesn't expand values", {
  skip_if_not_installed("tidyr")
  expand <- tidyr::expand

  df <- tibble(x = 1:2, y = 1:2)
  expect_equal(
    expand(memdb_frame(!!!df), nesting(x, y)) %>%
      collect(),
    df
  )

  df_lazy <- lazy_frame(!!!df)
  expect_snapshot(df_lazy %>% expand(nesting(x, y)))
})

test_that("expand accepts expressions", {
  skip_if_not_installed("tidyr")
  expand <- tidyr::expand

  df <- lazy_frame(x = 1)
  expect_snapshot(expand(df, round(x / 2)))
  expect_snapshot(expand(df, nesting(x_half = round(x / 2), x1 = x + 1)))
})

test_that("expand respects groups", {
  skip_if_not_installed("tidyr")
  expand <- tidyr::expand

  df <- tibble(
    a = c(1L, 1L, 2L),
    b = c(1L, 2L, 1L),
    c = c("b", "a", "a")
  )
  expect_equal(
    memdb_frame(!!!df) %>% group_by(a) %>% expand(b, c) %>% collect(),
    tibble(
      a = c(1, 1, 1, 1, 2),
      b = c(1, 1, 2, 2, 1),
      c = c("b", "a", "b", "a", "a")
    ) %>%
      group_by(a)
  )

  df_lazy <- lazy_frame(!!!df)
  expect_snapshot(df_lazy %>% group_by(a) %>% expand(b, c))
})

test_that("NULL inputs", {
  skip_if_not_installed("tidyr")
  expand <- tidyr::expand

  expect_snapshot(expand(lazy_frame(x = 1), x, y = NULL))
})

test_that("expand() errors when expected", {
  skip_if_not_installed("tidyr")
  expand <- tidyr::expand

  expect_snapshot_error(expand(memdb_frame(x = 1)))
  expect_snapshot_error(expand(memdb_frame(x = 1), x = NULL))
})

test_that("expand respect .name_repair", {
  skip_if_not_installed("tidyr")
  expand <- tidyr::expand

  expect_named(
    memdb_frame(x = integer(), z = integer()) %>%
      expand(x, z = x, nesting(x), nesting(z), .name_repair = "unique") %>%
      collect(),
    c("x...1", "z...2", "x...3", "z...4")
  )
})

# replace_na --------------------------------------------------------------

test_that("replace_na replaces missing values", {
  skip_if_not_installed("tidyr")
  replace_na <- tidyr::replace_na

  expect_equal(
    memdb_frame(x = c(1, NA), y = c(NA, "b")) %>%
      replace_na(list(x = 0, y = "unknown")) %>%
      collect(),
    tibble(
      x = c(1, 0),
      y = c("unknown", "b")
    )
  )

  expect_snapshot(
    lazy_frame(x = 1, y = "a") %>% replace_na(list(x = 0, y = "unknown"))
  )
})

test_that("replace_na ignores missing columns", {
  skip_if_not_installed("tidyr")
  replace_na <- tidyr::replace_na

  expect_snapshot(lazy_frame(x = 1) %>% replace_na(list(not_there = 0)))
})

# complete ----------------------------------------------------------------

test_that("complete completes missing combinations", {
  skip_if_not_installed("tidyr")
  complete <- tidyr::complete
  replace_na <- tidyr::replace_na

  df <- tibble(
    x = 1:2,
    y = 1:2,
    z = c("a", "b")
  )

  expect_equal(
    memdb_frame(!!!df) %>% complete(x, y, fill = list(z = "c")) %>% collect(),
    tibble(
      x = c(1, 1, 2, 2),
      y = c(1, 2, 1, 2),
      z = c("a", "c", "c", "b")
    )
  )

  df_lazy <- lazy_frame(!!!df)
  expect_snapshot(df_lazy %>% complete(x, y, fill = list(z = "c")))
})
