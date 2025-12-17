test_that("remote_table returns name when it makes sense", {
  mf <- copy_to_test("sqlite", tibble(x = 5), name = "refxiudlph")

  # produces name after `group_by()`
  expect_equal(
    mf |> group_by(x) |> remote_table(),
    table_path("`refxiudlph`")
  )

  # produces name after unarranging
  expect_equal(
    mf |> arrange(x) |> arrange() |> remote_table(),
    table_path("`refxiudlph`")
  )

  # produces name after compute()
  expect_false(is_null(
    mf |> mutate(x = x + 1) |> compute() |> remote_table()
  ))
})

test_that("remote_table returns null for computed tables", {
  mf <- copy_to_test("sqlite", tibble(x = 5, y = 1), name = "refxiudlph")
  expect_equal(remote_table(mf), table_path("`refxiudlph`"))

  expect_null(mf |> filter(x == 3) |> remote_table())
  expect_null(mf |> distinct(x) |> remote_table())
  expect_null(mf |> mutate(x = x + 1) |> remote_table())
  expect_null(mf |> select(x) |> remote_table())
  expect_null(mf |> relocate(y, x) |> remote_table())
  expect_null(mf |> head(3) |> remote_table())

  expect_null(left_join(mf, mf, by = "x") |> remote_table())
  lf <- lazy_frame(x = 1)
  expect_null(lf |> remote_table())
  expect_null(lf |> group_by(x) |> remote_table())

  lf <- lazy_frame(x = 1)
  expect_equal(lf |> remote_table(null_if_local = FALSE), table_path('"df"'))
  expect_equal(
    lf |> group_by(x) |> remote_table(null_if_local = FALSE),
    table_path('"df"')
  )
})

test_that("remote_name and remote_table can handle different table identifiers", {
  test_remote_table <- function(
    x,
    exp_tbl = as_table_path(x, simulate_dbi())
  ) {
    lf <- lazy_frame(x = 1, .name = x)
    expect_equal(remote_table(lf, null_if_local = FALSE), exp_tbl)
    expect_equal(remote_name(lf, null_if_local = FALSE), "tbl")
  }

  test_remote_table("tbl")
  test_remote_table(ident("tbl"))
  test_remote_table(in_schema("schema", "tbl"))
  test_remote_table(in_catalog("catalog", "schema", "tbl"))
  test_remote_table(DBI::Id(
    catalog = "catalog",
    schema = "schema",
    table = "tbl"
  ))
  test_remote_table(ident_q("schema.tbl"))
  test_remote_table(I("schema.tbl"))
})

test_that("can retrieve query, src and con metadata", {
  mf <- local_memdb_frame("df", x = 5)

  expect_s4_class(remote_con(mf), "DBIConnection")
  expect_s3_class(remote_src(mf), "src_sql")
  expect_s3_class(remote_query(mf), "sql")
  expect_type(remote_query_plan(mf), "character")
})

test_that("last_sql() retrieves the most recent query", {
  lf <- lazy_frame(x = 1:3, y = c("a", "b", "c"))

  lf |> filter(x > 1) |> show_query()
  expect_match(last_sql(), "WHERE")

  lf |> mutate(z = x + 1) |> show_query()
  expect_match(last_sql(), "\\+ 1")
})
