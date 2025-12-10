# build_sql() ------------------------------------------------------------------

test_that("build_sql() is deprecated", {
  con <- simulate_dbi()
  expect_snapshot(
    build_sql("SELECT * FROM TABLE", con = con)
  )
})

test_that("build_sql() requires connection", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- ident("TABLE")
  expect_snapshot(error = TRUE, build_sql("SELECT * FROM ", x))
})

# glue_sql() -------------------------------------------------------------------

test_that("glue_sql() interpolates .tbl correctly", {
  con <- simulate_dbi()
  expect_equal(glue_sql2("{.tbl 'table'}", .con = con), sql("`table`"))
  tbl <- "table"
  expect_equal(glue_sql2("{.tbl tbl}", .con = con), sql("`table`"))
  tbl <- ident("table")
  expect_equal(glue_sql2("{.tbl tbl}", .con = con), sql("`table`"))
  tbl <- in_schema("schema", "table")
  expect_equal(glue_sql2("{.tbl tbl}", .con = con), sql("`schema`.`table`"))
  tbl <- in_catalog("catalog", "schema", "table")
  expect_equal(
    glue_sql2("{.tbl tbl}", .con = con),
    sql("`catalog`.`schema`.`table`")
  )
})

test_that("glue_sql() handles type casting", {
  con <- simulate_dbi()
  x <- "x"

  expect_equal(glue_sql2("{x}", .con = con), sql("'x'"))
  expect_equal(glue_sql2("{.id x}", .con = con), sql("`x`"))
  expect_equal(glue_sql2("{.sql x}", .con = con), sql("x"))

  tbl1 <- "table"
  tbl2 <- I("schema.table")
  tbl3 <- in_schema("schema", "table")
  expect_equal(glue_sql2(con, "{.tbl tbl1}"), sql("`table`"))
  expect_equal(glue_sql2(con, "{.tbl tbl2}"), sql("schema.table"))
  expect_equal(glue_sql2(con, "{.tbl tbl3}"), sql("`schema`.`table`"))
})

test_that("glue_sql() can collapse with and without parens", {
  con <- simulate_dbi()
  x <- c("a", "b")

  expect_equal(glue_sql2("{.sql x}", .con = con), sql("a, b"))
  expect_equal(glue_sql2("{.sql x*}", .con = con), sql("(a, b)"))
})


test_that("gives informative errors", {
  con <- simulate_dbi()
  x <- 1
  expect_snapshot(error = TRUE, {
    glue_sql2(con, "{y*}")
    glue_sql2(con, "{1 + }")
    glue_sql2(con, "{.bar x}")
  })
})
