spec <- tibble(
  .name = c("x", "y"),
  .value = "val",
  key = c("x", "y")
)

spec1 <- tibble(.name = "x", .value = "val", key = "x")

test_that("can pivot all cols to wide", {
  expect_equal(
    local_memdb_frame(key = c("x", "y", "z"), val = 1:3) |>
      tidyr::pivot_wider(names_from = key, values_from = val) |>
      collect(),
    tibble(x = 1, y = 2, z = 3)
  )

  spec <- tibble(
    .name = c("x", "y", "z"),
    .value = "val",
    key = c("x", "y", "z")
  )

  expect_snapshot(
    lazy_frame(key = c("x", "y", "z"), val = 1:3) |>
      dbplyr_pivot_wider_spec(spec)
  )
})

test_that("non-pivoted cols are preserved", {
  df <- lazy_frame(a = 1, key = c("x", "y"), val = 1:2)

  expect_equal(
    dbplyr_pivot_wider_spec(df, spec) |> op_vars(),
    c("a", "x", "y")
  )
})

test_that("implicit missings turn into explicit missings", {
  df <- local_memdb_frame("df", a = 1:2, key = c("x", "y"), val = 1:2)

  expect_equal(
    df |>
      tidyr::pivot_wider(names_from = key, values_from = val) |>
      collect(),
    tibble(a = 1:2, x = c(1, NA), y = c(NA, 2))
  )

  expect_snapshot(
    df |>
      dbplyr_pivot_wider_spec(spec) |>
      show_query()
  )
})

test_that("error when overwriting existing column", {
  df <- local_memdb_frame(a = c(1, 1), key = c("a", "b"), val = c(1, 2))
  expect_snapshot(
    error = TRUE,
    tidyr::pivot_wider(df, names_from = key, values_from = val)
  )
})

test_that("`names_repair` happens after spec column reorganization (#1107)", {
  df <- local_memdb_frame(
    test = c("a", "b"),
    name = c("test", "test2"),
    value = c(1, 2)
  )

  out <- tidyr::pivot_wider(df, names_repair = ~ make.unique(.x)) |>
    collect()

  expect_identical(out$test, c("a", "b"))
  expect_identical(out$test.1, c(1, NA))
  expect_identical(out$test2, c(NA, 2))
})

test_that("minimal `names_repair` doesn't overwrite a value column that collides with key column (#1107)", {
  skip("`grouped_df()` needs a `name_repair` argument")
  # `collect.tbl_sql()` does not work with duplicated names
  df <- local_memdb_frame(
    test = c("a", "b"),
    name = c("test", "test2"),
    value = c(1, 2)
  )

  out <- tidyr::pivot_wider(df, names_repair = "minimal") |>
    collect()

  expect_identical(out[[1]], c("a", "b"))
  expect_identical(out[[2]], c(1, NA))
  expect_identical(out[[3]], c(NA, 2))
})

test_that("grouping is preserved", {
  df <- lazy_frame(a = 1, key = "x", val = 2)

  expect_equal(
    df |>
      dplyr::group_by(a) |>
      dbplyr_pivot_wider_spec(spec1) |>
      group_vars(),
    "a"
  )
})

# https://github.com/tidyverse/tidyr/issues/804
test_that("column with `...j` name can be used as `names_from`", {
  df <- local_memdb_frame(...8 = c("x", "y", "z"), val = 1:3)
  pv <- tidyr::pivot_wider(df, names_from = ...8, values_from = val) |>
    collect()
  expect_named(pv, c("x", "y", "z"))
})


# column names -------------------------------------------------------------

test_that("dbplyr_build_wider_spec can handle multiple columns", {
  df <- local_memdb_frame(x = c("X", "Y"), y = 1:2, a = 1:2, b = 1:2)

  expect_equal(
    dbplyr_build_wider_spec(df, x:y, a:b),
    tibble::tribble(
      ~.name  , ~.value , ~x  , ~y ,
      "a_X_1" , "a"     , "X" , 1L ,
      "a_Y_2" , "a"     , "Y" , 2L ,
      "b_X_1" , "b"     , "X" , 1L ,
      "b_Y_2" , "b"     , "Y" , 2L
    )
  )
})

test_that("pivot_wider handles NA column names consistent with tidyr", {
  df <- local_memdb_frame(id = "id", x = 1:3, y = c("A", NA, "B"))

  expect_equal(
    df |> tidyr::pivot_wider(names_from = y, values_from = x) |> collect(),
    df |> collect() |> tidyr::pivot_wider(names_from = y, values_from = x)
  )
})

# keys ---------------------------------------------------------

test_that("can override default keys", {
  df_db <- local_memdb_frame(
    row = c(1, 2, 3),
    name = c("Sam", "Sam", "Bob"),
    var = c("age", "height", "age"),
    value = c(10, 1.5, 20)
  )

  expect_equal(
    df_db |>
      tidyr::pivot_wider(
        id_cols = name,
        names_from = var,
        values_from = value
      ) |>
      collect(),
    tibble::tribble(
      ~name , ~age , ~height ,
      "Bob" ,   20 , NA      ,
      "Sam" ,   10 , 1.5
    )
  )
})

test_that("`id_cols = everything()` excludes `names_from` and `values_from`", {
  df <- local_memdb_frame(key = "x", name = "a", value = 1L)

  expect_identical(
    tidyr::pivot_wider(df, id_cols = everything()) |> collect(),
    tibble(key = "x", a = 1L)
  )

  spec <- dbplyr_build_wider_spec(df)

  expect_identical(
    dbplyr_pivot_wider_spec(df, spec, id_cols = everything()) |> collect(),
    tibble(key = "x", a = 1L)
  )
})

test_that("pivoting a zero row data frame drops `names_from` and `values_from` (#1249)", {
  df <- local_memdb_frame(
    key = character(),
    name = character(),
    value = integer()
  )

  expect_identical(
    tidyr::pivot_wider(df, names_from = name, values_from = value) |>
      collect(),
    tibble(key = character())
  )
})

test_that("known bug - building a wider spec with a zero row data frame loses `values_from` info (#1249)", {
  # We can't currently change this behavior in `pivot_wider_spec()`,
  # for fear of breaking backwards compatibility

  df <- local_memdb_frame(
    key = character(),
    name = character(),
    value = integer()
  )

  # Building the spec loses the fact that `value` was specified as `values_from`,
  # which would normally be in the `spec$.value` column
  spec <- dbplyr_build_wider_spec(df, names_from = name, values_from = value)

  # So pivoting with this spec accidentally keeps `value` around
  expect_identical(
    dbplyr_pivot_wider_spec(df, spec) |> collect(),
    tibble(key = character(), value = integer())
  )

  # If you specify `id_cols` to be the `key` column, it works right
  expect_identical(
    dbplyr_pivot_wider_spec(df, spec, id_cols = key) |> collect(),
    tibble(key = character())
  )

  # But `id_cols = everything()` won't work as intended, because we can't know
  # to remove `value` from `names(data)` before computing the tidy-selection
  expect_identical(
    dbplyr_pivot_wider_spec(df, spec, id_cols = everything()) |> collect(),
    tibble(key = character(), value = integer())
  )
})

# non-unique keys ---------------------------------------------------------

test_that("values_fn can be a single function", {
  df <- lazy_frame(a = c(1, 1, 2), key = c("x", "x", "x"), val = c(1, 10, 100))

  expect_snapshot(
    suppressWarnings(dbplyr_pivot_wider_spec(df, spec1, values_fn = sum))
  )
})

test_that("values_fn can be a formula", {
  df <- lazy_frame(a = c(1, 1, 2), key = c("x", "x", "x"), val = c(1, 10, 100))

  expect_snapshot(dbplyr_pivot_wider_spec(
    df,
    spec1,
    values_fn = ~ sum(.x, na.rm = TRUE)
  ))
})

test_that("values_fn can be a named list", {
  df <- lazy_frame(
    key = c("x", "x"),
    a = c(1, 2),
    b = c(3, 4)
  )

  spec <- tibble(
    .name = c("a_x", "b_x"),
    .value = c("a", "b"),
    key = "x"
  )

  dbplyr_pivot_wider_spec(
    df,
    spec,
    values_fn = list(a = sum, b = ~ sum(.x, na.rm = TRUE))
  )

  # must specify `values_fn` for every column
  expect_snapshot_error(
    dbplyr_pivot_wider_spec(df, spec, values_fn = list(a = sum))
  )
  # no function must be `NULL`
  expect_snapshot_error(
    dbplyr_pivot_wider_spec(df, spec, values_fn = list(a = sum, b = NULL))
  )
})

test_that("values_fn cannot be NULL", {
  df <- lazy_frame(a = 1, key = "x", val = 1)

  expect_snapshot(
    error = TRUE,
    dbplyr_pivot_wider_spec(df, spec1, values_fn = NULL)
  )
})


# unused -------------------------------------------------------------------

test_that("`unused_fn` can summarize unused columns (#990)", {
  df <- local_memdb_frame(
    id = c(1, 1, 2, 2),
    unused1 = c(1, 2, 4, 3),
    unused2 = c(11, 12, 14, 13),
    name = c("a", "b", "a", "b"),
    value = c(1, 2, 3, 4)
  )

  # By name
  suppressWarnings(
    res <- tidyr::pivot_wider(
      df,
      id_cols = id,
      unused_fn = list(unused1 = max)
    ) |>
      collect()
  )
  expect_equal(colnames(res), c("id", "a", "b", "unused1"))
  expect_identical(res$unused1, c(2, 4))

  # Globally
  suppressWarnings(
    res <- tidyr::pivot_wider(df, id_cols = id, unused_fn = min) |>
      collect()
  )
  expect_equal(colnames(res), c("id", "a", "b", "unused1", "unused2"))
  expect_identical(res$unused1, c(1, 3))
  expect_identical(res$unused2, c(11, 13))
})

test_that("`unused_fn` works with anonymous functions", {
  df <- local_memdb_frame(
    id = c(1, 1, 2, 2),
    unused = c(1, NA, 4, 3),
    name = c("a", "b", "a", "b"),
    value = c(1, 2, 3, 4)
  )

  res <- tidyr::pivot_wider(
    df,
    id_cols = id,
    unused_fn = ~ mean(.x, na.rm = TRUE)
  ) |>
    collect()
  expect_identical(res$unused, c(1, 3.5))
})

test_that("`unused_fn` is validated", {
  df <- local_memdb_frame("df", id = 1, unused = 1, name = "a", value = 1)

  expect_snapshot(
    (expect_error(tidyr::pivot_wider(df, id_cols = id, unused_fn = 1)))
  )
})

# can fill missing cells --------------------------------------------------

test_that("can fill in missing cells", {
  spec <- tibble(
    .name = c("x", "y"),
    .value = "value",
    name = c("x", "y")
  )
  df <- local_memdb_frame(
    g = c(1, 2),
    name = c("x", "y"),
    value = c(1, 2)
  )
  df_lazy <- lazy_frame(g = c(1, 2), name = c("x", "y"), value = c(1, 2))

  expect_equal(tidyr::pivot_wider(df) |> pull(x), c(1, NA))

  expect_equal(tidyr::pivot_wider(df, values_fill = 0) |> pull(x), c(1, 0))
  expect_snapshot(dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = 0))

  expect_equal(
    tidyr::pivot_wider(df, values_fill = list(value = 0)) |>
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
  df <- local_memdb_frame(
    g = c(1, 2),
    name = c("x", "y"),
    value = c(1, NA)
  )
  dbplyr_build_wider_spec(df)
  out <- tidyr::pivot_wider(df, values_fill = 0) |>
    collect()
  expect_equal(out$y, c(0, NA))
})

test_that("values_fill is checked", {
  lf <- lazy_frame(g = c(1, 2), name = c("x", "y"), value = c(1, NA))
  spec <- tibble(
    .name = c("x", "y"),
    .value = "value",
    name = .name
  )
  expect_snapshot(
    error = TRUE,
    dbplyr_pivot_wider_spec(lf, spec, values_fill = 1:2)
  )
})

# multiple values ----------------------------------------------------------

test_that("can pivot from multiple measure cols", {
  df <- local_memdb_frame(row = 1, var = c("x", "y"), a = 1:2, b = 3:4)
  pv <- tidyr::pivot_wider(df, names_from = var, values_from = c(a, b)) |>
    collect()

  expect_named(pv, c("row", "a_x", "a_y", "b_x", "b_y"))
  expect_equal(pv$a_x, 1)
  expect_equal(pv$b_y, 4)
})

test_that("column order in output matches spec", {
  df <- tibble::tribble(
    ~hw   , ~name  , ~mark , ~pr   ,
    "hw1" , "anna" ,    95 , "ok"  ,
    "hw2" , "anna" ,    70 , "meh" ,
  )

  # deliberately create weird order
  sp <- tibble::tribble(
    ~hw   , ~.value , ~.name     ,
    "hw1" , "mark"  , "hw1_mark" ,
    "hw1" , "pr"    , "hw1_pr"   ,
    "hw2" , "pr"    , "hw2_pr"   ,
    "hw2" , "mark"  , "hw2_mark" ,
  )

  pv <- dbplyr_pivot_wider_spec(lazy_frame(!!!df), sp)
  expect_equal(pv |> op_vars(), c("name", sp$.name))
})

test_that("cannot pivot lazy frames", {
  expect_snapshot(
    error = TRUE,
    tidyr::pivot_wider(lazy_frame(name = "x", value = 1))
  )
})

# multiple names ----------------------------------------------------------

test_that("can pivot multiple from multiple names", {
  x <- tibble(
    seq = c(1, 1, 2, 2),
    name = rep(c("id", "name"), 2),
    value = c("01", "curie", "02", "arrhenius")
  )

  expect_equal(
    local_memdb_frame(!!!x) |>
      tidyr::pivot_wider(
        names_from = c(name, seq),
        values_from = value
      ) |>
      collect(),
    tibble(id_1 = "01", name_1 = "curie", id_2 = "02", name_2 = "arrhenius")
  )
})


# pass through arguments --------------------------------------------------

test_that("can vary `names_from` values slowest (#839)", {
  df <- local_memdb_frame(
    name = c("name1", "name2"),
    value1 = c(1, 2),
    value2 = c(4, 5)
  )

  spec <- dbplyr_build_wider_spec(
    df,
    names_from = name,
    values_from = c(value1, value2)
  )

  expect_identical(
    spec$.name,
    c("value1_name1", "value1_name2", "value2_name1", "value2_name2")
  )

  spec <- dbplyr_build_wider_spec(
    df,
    names_from = name,
    values_from = c(value1, value2),
    names_vary = "slowest"
  )

  expect_identical(
    spec$.name,
    c("value1_name1", "value2_name1", "value1_name2", "value2_name2")
  )
})

test_that("`names_expand` does a cartesian expansion of `names_from` columns (#770)", {
  df <- local_memdb_frame(
    name1 = c("a", "b"),
    name2 = c("c", "d"),
    value = c(1, 2)
  )
  spec <- dbplyr_build_wider_spec(
    df,
    names_from = c(name1, name2),
    names_expand = TRUE
  )
  expect_identical(spec$.name, c("a_c", "a_d", "b_c", "b_d"))
})


# checks arguments --------------------------------------------------------

test_that("`names_from` must be supplied if `name` isn't in `data` (#1240)", {
  df <- local_memdb_frame("df", key = "x", val = 1)
  expect_snapshot((expect_error(tidyr::pivot_wider(df, values_from = val))))
})

test_that("`values_from` must be supplied if `value` isn't in `data` (#1240)", {
  df <- local_memdb_frame("df", key = "x", val = 1)
  expect_snapshot((expect_error(tidyr::pivot_wider(df, names_from = key))))
})

test_that("`names_from` must identify at least 1 column (#1240)", {
  df <- local_memdb_frame("df", key = "x", val = 1)
  expect_snapshot(
    (expect_error(tidyr::pivot_wider(
      df,
      names_from = starts_with("foo"),
      values_from = val
    )))
  )
})

test_that("`values_from` must identify at least 1 column (#1240)", {
  df <- local_memdb_frame("df", key = "x", val = 1)
  expect_snapshot(
    (expect_error(tidyr::pivot_wider(
      df,
      names_from = key,
      values_from = starts_with("foo")
    )))
  )
})

test_that("`id_expand` must be FALSE", {
  df <- lazy_frame(name = "x", value = 1)
  expect_snapshot(
    (expect_error(tidyr::pivot_wider(df, id_expand = TRUE)))
  )
})
