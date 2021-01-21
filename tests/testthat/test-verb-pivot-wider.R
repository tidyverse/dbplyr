spec <- tibble(
  .name = c("x", "y"),
  .value = "val",
  key = c("x", "y")
)

spec1 <- tibble(.name = "x", .value = "val", key = "x")

test_that("can pivot all cols to wide", {
  expect_equal(
    memdb_frame(key = c("x", "y", "z"), val = 1:3) %>%
      tidyr::pivot_wider(names_from = key, values_from = val) %>%
      collect(),
    tibble(x = 1, y = 2, z = 3)
  )

  spec <- tibble(
    .name = c("x", "y", "z"),
    .value = "val",
    key = c("x", "y", "z")
  )

  expect_snapshot(
    lazy_frame(key = c("x", "y", "z"), val = 1:3) %>%
      dbplyr_pivot_wider_spec(spec)
  )
})

test_that("non-pivoted cols are preserved", {
  df <- lazy_frame(a = 1, key = c("x", "y"), val = 1:2)

  expect_equal(
    dbplyr_pivot_wider_spec(df, spec) %>% op_vars(),
    c("a", "x", "y")
  )
})

test_that("implicit missings turn into explicit missings", {
  df <- memdb_frame(a = 1:2, key = c("x", "y"), val = 1:2)

  expect_equal(
    memdb_frame(a = 1:2, key = c("x", "y"), val = 1:2) %>%
      tidyr::pivot_wider(names_from = key, values_from = val) %>%
      collect(),
    tibble(a = 1:2, x = c(1, NA), y = c(NA, 2))
  )

  expect_snapshot(
    lazy_frame(a = 1:2, key = c("x", "y"), val = 1:2) %>%
      dbplyr_pivot_wider_spec(spec)
  )
})

test_that("error when overwriting existing column", {
  df <- memdb_frame(
    a = c(1, 1),
    key = c("a", "b"),
    val = c(1, 2)
  )
  expect_snapshot_error(
    tidyr::pivot_wider(df, names_from = key, values_from = val)
  )
})

test_that("grouping is preserved", {
  df <- lazy_frame(a = 1, key = "x", val = 2)

  expect_equal(
    df %>%
      dplyr::group_by(a) %>%
      dbplyr_pivot_wider_spec(spec1) %>%
      group_vars(),
    "a"
  )
})

# https://github.com/tidyverse/tidyr/issues/804
test_that("column with `...j` name can be used as `names_from`", {
  df <- memdb_frame(...8 = c("x", "y", "z"), val = 1:3)
  pv <- tidyr::pivot_wider(df, names_from = ...8, values_from = val) %>% collect()
  expect_named(pv, c("x", "y", "z"))
})


# column names -------------------------------------------------------------

test_that("dbplyr_build_wider_spec can handle multiple columns", {
  df <- memdb_frame(
    x = c("X", "Y"),
    y = 1:2,
    a = 1:2,
    b = 1:2
  )

  expect_equal(
    dbplyr_build_wider_spec(df, x:y, a:b),
    tibble::tribble(
      ~.name, ~.value,  ~x, ~y,
      "a_X_1",     "a", "X", 1L,
      "a_Y_2",     "a", "Y", 2L,
      "b_X_1",     "b", "X", 1L,
      "b_Y_2",     "b", "Y", 2L
    )
  )
})

# keys ---------------------------------------------------------

test_that("can override default keys", {
  df <- tibble::tribble(
    ~row, ~name, ~var, ~value,
    1,    "Sam", "age", 10,
    2,    "Sam", "height", 1.5,
    3,    "Bob", "age", 20,
  )

  df_db <- memdb_frame(!!!df)

  expect_equal(
    df_db %>%
      tidyr::pivot_wider(id_cols = name, names_from = var, values_from = value) %>%
      collect(),
    tibble::tribble(
      ~name, ~age, ~height,
      "Bob",   20,      NA,
      "Sam",   10,     1.5
    )
  )
})


# non-unqiue keys ---------------------------------------------------------

test_that("values_fn can be a single function", {
  df <- lazy_frame(a = c(1, 1, 2), key = c("x", "x", "x"), val = c(1, 10, 100))

  expect_snapshot(dbplyr_pivot_wider_spec(df, spec1, values_fn = sum))
})

test_that("values_fn cannot be NULL", {
  df <- lazy_frame(a = 1, key = "x", val = 1)

  expect_snapshot_error(dbplyr_pivot_wider_spec(df, spec1, values_fn = NULL))
})

# can fill missing cells --------------------------------------------------

test_that("can fill in missing cells", {
  df <- memdb_frame(g = c(1, 2), name = c("x", "y"), value = c(1, 2))
  df_lazy <- lazy_frame(g = c(1, 2), name = c("x", "y"), value = c(1, 2))

  expect_equal(tidyr::pivot_wider(df) %>% pull(x), c(1, NA))

  expect_equal(tidyr::pivot_wider(df, values_fill = 0) %>% pull(x), c(1, 0))
  expect_snapshot(dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = 0))

  expect_equal(
    tidyr::pivot_wider(df, values_fill = list(value = 0)) %>%
      pull(x),
    c(1, 0)
  )
  expect_snapshot(
    dbplyr_pivot_wider_spec(
      df_lazy,
      spec,
      values_fill = list(value = 0)
    )
  )
})

test_that("values_fill only affects missing cells", {
  df <- memdb_frame(g = c(1, 2), name = c("x", "y"), value = c(1, NA))
  out <- tidyr::pivot_wider(df, values_fill = 0) %>%
    collect()
  expect_equal(out$y, c(0, NA))
})

# multiple values ----------------------------------------------------------

test_that("can pivot from multiple measure cols", {
  df <- memdb_frame(row = 1, var = c("x", "y"), a = 1:2, b = 3:4)
  pv <- tidyr::pivot_wider(df, names_from = var, values_from = c(a, b)) %>%
    collect()

  expect_named(pv, c("row", "a_x", "a_y", "b_x", "b_y"))
  expect_equal(pv$a_x, 1)
  expect_equal(pv$b_y, 4)
})

test_that("column order in output matches spec", {
  df <- tibble::tribble(
    ~hw,   ~name,  ~mark,   ~pr,
    "hw1", "anna",    95,  "ok",
    "hw2", "anna",    70, "meh",
  )

  # deliberately create weird order
  sp <- tibble::tribble(
    ~hw, ~.value,  ~.name,
    "hw1", "mark", "hw1_mark",
    "hw1", "pr",   "hw1_pr",
    "hw2", "pr",   "hw2_pr",
    "hw2", "mark", "hw2_mark",
  )

  pv <- dbplyr_pivot_wider_spec(lazy_frame(!!!df), sp)
  expect_equal(pv %>% op_vars(), c("name", sp$.name))
})

test_that("cannot pivot lazy frames", {
  expect_snapshot_error(tidyr::pivot_wider(lazy_frame(name = "x", value = 1)))
})
