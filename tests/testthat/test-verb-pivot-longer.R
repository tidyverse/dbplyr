test_that("can pivot all cols to long", {
  pv <- memdb_frame(x = 1:2, y = 3:4) %>%
    tidyr::pivot_longer(x:y)

  expect_equal(
    pv %>% collect(),
    tibble(
      name = c("x", "x", "y", "y"),
      value = 1:4
    )
  )

  expect_snapshot(
    lazy_frame(x = 1:2, y = 3:4) %>%
      tidyr::pivot_longer(x:y)
  )
})

test_that("can add multiple columns from spec", {
  # add columns `a` and `b`
  sp <- tibble(
    .name = c("x", "y"),
    .value = "v",
    a = 11:12,
    b = 13:14
  )
  pv <- lazy_frame(x = 1:2, y = 3:4) %>%
    dbplyr_pivot_longer_spec(df_db, spec = sp)

  expect_equal(colnames(pv), c("a", "b", "v"))
  expect_snapshot(pv)
})

test_that("preserves original keys", {
  # preserves `x`
  pv <- lazy_frame(x = 1:2, y = 2, z = 1:2) %>%
    tidyr::pivot_longer(y:z)

  expect_equal(colnames(pv), c("x", "name", "value"))
  expect_snapshot(pv)
})

test_that("can drop missing values", {
  expect_snapshot(
    lazy_frame(x = c(1, NA), y = c(NA, 2)) %>%
      tidyr::pivot_longer(x:y, values_drop_na = TRUE)
  )
})

test_that("can handle missing combinations", {
  df <- tibble::tribble(
    ~id, ~x_1, ~x_2, ~y_2,
    "A",    1,    2,  "a",
    "B",    3,    4,  "b",
  )

  df_db <- memdb_frame(!!!df)

  pv_db <- tidyr::pivot_longer(
    df_db,
    -id,
    names_to = c(".value", "n"),
    names_sep = "_"
  )
  pv <- pv_db %>% collect()

  expect_named(pv, c("id", "n", "x", "y"))
  expect_equal(pv$x, c(1, 3, 2, 4))
  expect_equal(pv$y, c(NA, NA, "a", "b"))

  sql <- tidyr::pivot_longer(
    lazy_frame(!!!df),
    -id,
    names_to = c(".value", "n"),
    names_sep = "_"
  )
  expect_snapshot(sql)
})

test_that("can override default output column type", {
  expect_snapshot(
    lazy_frame(x = 1) %>%
      tidyr::pivot_longer(x, values_transform = list(value = as.character))
  )
})

test_that("can pivot to multiple measure cols", {
  df <- lazy_frame(x = "x", y = 1)
  sp <- tibble::tribble(
    ~.name, ~.value, ~row,
    "x", "X", 1,
    "y", "Y", 1,
  )

  pv <- dbplyr_pivot_longer_spec(df, sp)
  expect_equal(colnames(pv), c("row", "X", "Y"))
  expect_snapshot(pv)
})

test_that("original col order is preserved", {
  df <- tibble::tribble(
    ~id, ~z_1, ~y_1, ~x_1, ~z_2,  ~y_2, ~x_2,
    "A",    1,    2,    3,     4,    5,    6,
    "B",    7,    8,    9,    10,   11,   12,
  ) %>%
    tbl_lazy()

  expect_equal(
    df %>%
      tidyr::pivot_longer(-id, names_to = c(".value", "n"), names_sep = "_") %>%
      colnames(),
    c("id", "n", "z", "y", "x")
  )
})

test_that(".value can be at any position in `names_to`", {
  samp <- tibble(
    i = 1:4,
    y_t1 = rnorm(4),
    y_t2 = rnorm(4),
    z_t1 = rep(3, 4),
    z_t2 = rep(-2, 4),
  )

  value_first <- tidyr::pivot_longer(
    lazy_frame(!!!samp), -i,
    names_to = c(".value", "time"), names_sep = "_"
  )

  expect_snapshot(value_first)

  samp2 <- dplyr::rename(samp, t1_y = y_t1,
                               t2_y = y_t2,
                               t1_z = z_t1,
                               t2_z = z_t2)

  value_second <- tidyr::pivot_longer(
    lazy_frame(!!!samp2), -i,
    names_to = c("time", ".value"), names_sep = "_"
  )

  expect_snapshot(value_second)

  cols <- c("i", "time", "y", "z")
  expect_equal(colnames(value_first), cols)
  expect_equal(colnames(value_second), cols)
})

test_that("grouping is preserved", {
  df <- memdb_frame(g = 1, x1 = 1, x2 = 2)
  out <- df %>%
    group_by(g) %>%
    tidyr::pivot_longer(x1:x2, names_to = "x", values_to = "v")
  expect_equal(group_vars(out), "g")
})
