test_that("can pivot all cols to long", {
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

  pv <- memdb_frame(x = 1:2, y = 3:4) %>%
    pivot_longer(x:y)

  expect_equal(
    pv %>% collect(),
    tibble(
      name = c("x", "x", "y", "y"),
      value = 1:4
    )
  )

  expect_snapshot(pv %>% show_query())
})

test_that("can add multiple columns from spec", {
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

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
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

  # preserves `x`
  pv <- lazy_frame(x = 1:2, y = 2, z = 1:2) %>%
    pivot_longer(y:z)

  expect_equal(colnames(pv), c("x", "name", "value"))
  expect_snapshot(pv)
})

test_that("can drop missing values", {
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

  expect_snapshot(
    lazy_frame(x = c(1, NA), y = c(NA, 2)) %>%
      pivot_longer(x:y, values_drop_na = TRUE)
  )
})

test_that("can handle missing combinations", {
  skip("not yet adapted to dbplyr")
  df <- tibble::tribble(
    ~id, ~x_1, ~x_2, ~y_2,
    "A",    1,    2,  "a",
    "B",    3,    4,  "b",
  )
  pv <- pivot_longer(
    memdb_frame(df),
    -id,
    names_to = c(".value", "n"),
    names_sep = "_"
  ) %>%
    collect()

  expect_named(pv, c("id", "n", "x", "y"))
  expect_equal(pv$x, c(1, 3, 2, 4))
  expect_equal(pv$y, c(NA, NA, "a", "b"))
})

test_that("can override default output column type", {
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

  expect_snapshot(
    lazy_frame(x = 1) %>%
      pivot_longer(x, values_transform = list(value = as.character))
  )
})

test_that("can pivot to multiple measure cols", {
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

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
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

  df <- tibble::tribble(
    ~id, ~z_1, ~y_1, ~x_1, ~z_2,  ~y_2, ~x_2,
    "A",    1,    2,    3,     4,    5,    6,
    "B",    7,    8,    9,    10,   11,   12,
  ) %>%
    tbl_lazy()

  expect_equal(
    df %>%
      pivot_longer(-id, names_to = c(".value", "n"), names_sep = "_") %>%
      colnames(),
    c("id", "n", "z", "y", "x")
  )
})

test_that(".value can be at any position in `names_to`", {
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

  samp <- tibble(
    i = 1:4,
    y_t1 = rnorm(4),
    y_t2 = rnorm(4),
    z_t1 = rep(3, 4),
    z_t2 = rep(-2, 4),
  )

  value_first <- pivot_longer(
    lazy_frame(!!!samp), -i,
    names_to = c(".value", "time"), names_sep = "_"
  )

  expect_snapshot(value_first)

  samp2 <- dplyr::rename(samp, t1_y = y_t1,
                               t2_y = y_t2,
                               t1_z = z_t1,
                               t2_z = z_t2)

  value_second <- pivot_longer(
    lazy_frame(!!!samp2), -i,
    names_to = c("time", ".value"), names_sep = "_"
  )

  expect_snapshot(value_second)

  cols <- c("i", "time", "y", "z")
  expect_equal(colnames(value_first), cols)
  expect_equal(colnames(value_second), cols)
})

test_that("grouping is preserved", {
  skip_if_not_installed("tidyr")
  withr::local_package("tidyr")

  df <- memdb_frame(g = 1, x1 = 1, x2 = 2)
  out <- df %>%
    group_by(g) %>%
    pivot_longer(x1:x2, names_to = "x", values_to = "v")
  expect_equal(group_vars(out), "g")
})

# spec --------------------------------------------------------------------

test_that("validates inputs", {
  df <- memdb_frame(x = 1)
  expect_error(build_longer_spec(df, x, values_to = letters[1:2]),
    class = "vctrs_error_assert"
  )
})

test_that("no names doesn't generate names", {
  df <- memdb_frame(x = 1)
  expect_equal(
    colnames(build_longer_spec(df, x, names_to = character())),
    c(".name", ".value")
  )
})

test_that("multiple names requires names_sep/names_pattern", {
  df <- memdb_frame(x_y = 1)
  expect_error(
    build_longer_spec(df, x_y, names_to = c("a", "b")),
    "multiple names"
  )

  expect_error(
    build_longer_spec(df, x_y,
      names_to = c("a", "b"),
      names_sep = "x",
      names_pattern = "x"
    ),
    "one of `names_sep` or `names_pattern"
  )
})

test_that("names_sep generates correct spec", {
  df <- memdb_frame(x_y = 1)
  sp <- build_longer_spec(df, x_y, names_to = c("a", "b"), names_sep = "_")

  expect_equal(sp$a, "x")
  expect_equal(sp$b, "y")
})

test_that("names_sep fails with single name", {
  df <- memdb_frame(x_y = 1)
  expect_error(build_longer_spec(df, x_y, names_to = "x", names_sep = "_"), "`names_sep`")
})

test_that("names_pattern generates correct spec", {
  df <- memdb_frame(zx_y = 1)
  sp <- build_longer_spec(df, zx_y, names_to = c("a", "b"), names_pattern = "z(.)_(.)")
  expect_equal(sp$a, "x")
  expect_equal(sp$b, "y")

  sp <- build_longer_spec(df, zx_y, names_to = "a", names_pattern = "z(.)")
  expect_equal(sp$a, "x")
})

test_that("names_to can override value_to", {
  df <- memdb_frame(x_y = 1)
  sp <- build_longer_spec(df, x_y, names_to = c("a", ".value"), names_sep = "_")

  expect_equal(sp$.value, "y")
})

test_that("names_prefix strips off from beginning", {
  df <- memdb_frame(zzyz = 1)
  sp <- build_longer_spec(df, zzyz, names_prefix = "z")

  expect_equal(sp$name, "zyz")
})

test_that("can cast to custom type", {
  df <- memdb_frame(w1 = 1)
  sp <- build_longer_spec(df, w1,
    names_prefix = "w",
    names_transform = list(name = as.integer)
  )

  expect_equal(sp$name, 1L)
})

test_that("Error if the `col` can't be selected.", {
  expect_error(pivot_longer(memdb_frame(!!!iris), matches("foo")), "select at least one")
})
