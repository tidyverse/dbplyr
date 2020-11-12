test_that("expand completes all values", {
  df <- memdb_frame(x = 1:2, y = 1:2)
  out <- expand(df, x, y) %>% collect()
  expect_equal(nrow(out), 4)
})

test_that("multiple variables in one arg doesn't expand", {
  skip("doesn't make sense in db?")
  df <- data.frame(x = 1:2, y = 1:2)
  out <- expand(df, c(x, y))
  expect_equal(nrow(out), 2)
})

test_that("nesting doesn't expand values", {
  df <- tibble(x = 1:2, y = 1:2)
  expect_equal(
    expand(memdb_frame(!!!df), nesting(x, y)) %>%
      collect(),
    df
  )
})

test_that("unnamed data frames are flattened", {
  skip("do not understand test")
  df <- data.frame(x = 1:2, y = 1:2)
  out <- expand(df, nesting(x, y))
  expect_equal(out$x, df$x)

  out <- crossing(df)
  expect_equal(out$x, df$x)
})

test_that("named data frames are not flattened", {
  skip("doesn't make sense in db?")
  df <- tibble(x = 1:2, y = 1:2)
  out <- expand(df, x = nesting(x, y))
  expect_equal(out$x, df)

  out <- crossing(x = df)
  expect_equal(out$x, df)
})

test_that("expand works with non-standard col names", {
  df <- memdb_frame(` x ` = 1:2, `/y` = 1:2)
  out <- expand(df, ` x `, `/y`) %>% collect()
  expect_equal(nrow(out), 4)
})

test_that("expand excepts expressions", {
  df <- expand(memdb_frame(x = 1:4), y = round(x / 2)) %>% collect()
  expect_equal(df, tibble(y = 1:2))
})

test_that("expand respects groups", {
  skip("doesn't work because distinct ignores groups")
  # https://github.com/tidyverse/dbplyr/issues/535
  local_options(lifecycle_verbosity = "quiet")

  df <- tibble(
    a = c(1L, 1L, 2L),
    b = c(1L, 2L, 1L),
    c = c(2L, 1L, 1L)
  )
  out <- memdb_frame(!!!df) %>% dplyr::group_by(a) %>% expand(b, c)

  expect_equal(out$data[[1]], crossing(b = 1:2, c = 1:2))
  expect_equal(out$data[[2]], tibble(b = 1L, c = 1L))
})

test_that("preserves ordered factors", {
  skip("doesn't make sense in db?")
  df <- tibble(a = ordered("a"))
  out <- expand(df, a)
  expect_equal(df$a, ordered("a"))
})

test_that("NULL inputs", {
  tb <- tibble(x = 1:5)
  expect_equal(
    expand(memdb_frame(!!!tb), x, y = NULL) %>%
      collect(),
    tb
  )
  # expect_equal(nesting(x = tb$x, y = NULL), tb)
  # expect_equal(crossing(x = tb$x, y = NULL), tb)
})

test_that("zero length input gives zero length output", {
  tb <- tibble(x = character())
  expect_equal(
    expand(memdb_frame(!!!tb), x) %>% collect(),
    tb
  )

  skip("do empty tables make sense in a db?")
  expect_equal(
    expand(memdb_frame(!!!tb), y = NULL) %>% collect(),
    tibble()
  )

  # expect_equal(
  #   expand_grid(x = integer(), y = 1:3),
  #   tibble(x = integer(), y = integer())
  # )
})

test_that("expand() reconstructs input dots is empty", {
  skip("do empty tables make sense in a db?")

  expect_is(expand(mtcars), "data.frame")
  expect_is(expand(as_tibble(mtcars)), "tbl_df")
})

test_that("crossing/nesting/expand respect .name_repair", {
  df <- tibble(x = 1:2)
  expect_named(
    memdb_frame(!!!df) %>%
      expand(x, y = x, z = x, nesting(y = x, x), .name_repair = "unique") %>%
      collect(),
    c("x...1", "y...2", "z", "y...4", "x...5")
  )
})
