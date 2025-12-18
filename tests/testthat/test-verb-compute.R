test_that("compute can create indexes", {
  # integration test to ensure that indexes argument passed all the way through
  db <- local_memdb_frame("db1", x = 5:1, y = 1:5, z = 10)

  db |> compute(indexes = c("x", "y"), name = "db2")
  indices <- DBI::dbGetQuery(memdb(), "PRAGMA index_list('db2');")
  expect_equal(indices$name, c("db2_y", "db2_x"))
  expect_equal(indices$unique, c(0, 0))

  db |> compute(unique_indexes = c("x", "y"), name = "db3")
  indices <- DBI::dbGetQuery(memdb(), "PRAGMA index_list('db3');")
  expect_equal(indices$name, c("db3_y", "db3_x"))
  expect_equal(indices$unique, c(1, 1))
})

test_that("unique index fails if values are duplicated", {
  mf <- local_memdb_frame(x = 5:1, y = "a")
  expect_error(compute(mf, unique_indexes = "y"))
})

test_that("index fails if columns are missing", {
  mf <- local_memdb_frame(x = 1)
  expect_snapshot({
    (expect_error(compute(mf, indexes = list(c("y", "x", "z"), "a"))))
    (expect_error(compute(mf, unique_indexes = list(c("y", "x", "z"), "a"))))
  })
})

test_that("compute creates correct column names", {
  out <- local_memdb_frame(x = 1) |>
    group_by(x) |>
    summarise(n = n()) |>
    compute() |>
    collect()

  expect_equal(out, tibble(x = 1, n = 1L))
})

test_that("compute keeps window and groups", {
  out <- local_memdb_frame(x = 1, y = 1) |>
    window_order(x) |>
    group_by(x, y) |>
    summarise(n = n(), .groups = "drop_last") |>
    compute()

  expect_equal(op_sort(out), list(quo(x)))
  expect_equal(op_grps(out), "x")
})

test_that("compute can handle named name", {
  con <- simulate_dbi()
  expect_equal(
    local_memdb_frame(x = 1:10) |>
      compute(name = c(x = unique_table_name())) |>
      collect(),
    tibble(x = 1:10)
  )
})

test_that("compute can handle schema", {
  df <- local_memdb_frame("db1", x = 1:10)
  withr::defer(DBI::dbRemoveTable(memdb(), "db1"))

  expect_equal(
    df |>
      compute(name = in_schema("main", "db1"), temporary = FALSE) |>
      collect(),
    tibble(x = 1:10)
  )

  # errors because name already exists
  expect_snapshot(
    df |> compute(name = in_schema("main", "db1"), temporary = FALSE),
    transform = snap_transform_dbi,
    error = TRUE
  )
})

test_that("compute(temporary = FALSE) without a name is deprecated", {
  df <- local_memdb_frame(x = 1:10)

  expect_snapshot_warning(df |> compute(temporary = FALSE))
})

test_that("sorting preserved across compute", {
  df1 <- local_memdb_frame(x = sample(10)) |> arrange(x)

  df2 <- compute(df1)
  expect_equal(get_expr(op_sort(df2)[[1]]), quote(x))
})
