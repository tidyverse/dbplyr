test_that("remote_table returns name when it makes sense", {
  mf <- local_memdb_frame("refxiudlph", x = 5)

  # produces name after `group_by()`
  expect_equal(
    mf |> group_by(x) |> remote_table(),
    sql("`refxiudlph`")
  )

  # produces name after compute()
  expect_false(is_null(
    mf |> mutate(x = x + 1) |> compute() |> remote_table()
  ))
})

test_that("remote_table returns null for computed tables", {
  mf <- local_memdb_frame("refxiudlph", x = 5, y = 1)
  expect_equal(remote_table(mf), sql("`refxiudlph`"))

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
  expect_equal(lf |> remote_table(null_if_local = FALSE), sql('"df"'))
  expect_equal(
    lf |> group_by(x) |> remote_table(null_if_local = FALSE),
    sql('"df"')
  )
})

test_that("remote_name and remote_table can handle different table identifiers", {
  test_remote_table <- function(x, exp_tbl) {
    lf <- lazy_frame(x = 1, .name = x)
    expect_equal(remote_table(lf, null_if_local = FALSE), exp_tbl)
    expect_equal(remote_name(lf, null_if_local = FALSE), "tbl")
  }

  test_remote_table("tbl", sql('"tbl"'))
  test_remote_table(ident("tbl"), sql('"tbl"'))
  test_remote_table(in_schema("schema", "tbl"), sql('"schema"."tbl"'))
  test_remote_table(
    in_catalog("catalog", "schema", "tbl"),
    sql('"catalog"."schema"."tbl"')
  )
  test_remote_table(
    DBI::Id(catalog = "catalog", schema = "schema", table = "tbl"),
    sql('"catalog"."schema"."tbl"')
  )
  test_remote_table(ident_q("schema.tbl"), sql("schema.tbl"))
  test_remote_table(I("schema.tbl"), sql("schema.tbl"))
})

test_that("remote_name() returns NULL when there is no single table name", {
  mf <- local_memdb_frame("ulfxutwbnh", x = 1)

  # NULL when lazy query no longer corresponds to a single table
  expect_null(mf |> filter(x == 1) |> remote_name())
  expect_null(mf |> mutate(y = x + 1) |> remote_name())
  expect_null(left_join(mf, mf, by = "x") |> remote_name())

  # NULL when source is a custom sql() query
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "mytbl", data.frame(x = 1))
  custom <- tbl(con, sql("SELECT * FROM mytbl"))
  expect_null(remote_name(custom))
})

test_that("remote_table() result can be inlined into build_sql()", {
  con <- dialect_ansi()
  lf <- lazy_frame(x = 1, .name = in_schema("myschema", "mytbl"), con = con)

  expect_equal(
    build_sql(
      "SELECT * FROM ",
      remote_table(lf, null_if_local = FALSE),
      con = con
    ),
    sql('SELECT * FROM "myschema"."mytbl"')
  )
})

test_that("can retrieve query, src and con metadata", {
  mf <- local_memdb_frame(x = 5)

  expect_s4_class(remote_con(mf), "DBIConnection")
  expect_s3_class(remote_src(mf), "src_sql")
  expect_s3_class(remote_query(mf), "sql")
  expect_type(remote_query_plan(mf), "character")
})

test_that("last_sql() retrieves the most recent query", {
  lf <- lazy_frame(x = 1:3, y = c("a", "b", "c"))

  capture.output(lf |> filter(x > 1) |> show_query())
  expect_match(last_sql(), "WHERE")

  capture.output(lf |> mutate(z = x + 1) |> show_query())
  expect_match(last_sql(), "\\+ 1")
})
