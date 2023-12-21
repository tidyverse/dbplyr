test_that("build_sql() requires connection", {
  x <- ident("TABLE")
  expect_snapshot(error = TRUE, build_sql("SELECT * FROM ", x))
})

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
  expect_equal(glue_sql2("{.tbl tbl}", .con = con), sql("`catalog`.`schema`.`table`"))
})

test_that("glue_sql() interpolates .from correctly", {
  con <- simulate_dbi()
  expect_equal(glue_sql2("{.from 'table'}", .con = con), sql("`table`"))
  tbl <- "table"
  expect_equal(glue_sql2("{.from tbl}", .con = con), sql("`table`"))
  tbl <- ident("table")
  expect_equal(glue_sql2("{.from tbl}", .con = con), sql("`table`"))
  tbl <- in_schema("schema", "table")
  expect_equal(glue_sql2("{.from tbl}", .con = con), sql("`schema`.`table`"))
  tbl <- in_catalog("catalog", "schema", "table")
  expect_equal(glue_sql2("{.from tbl}", .con = con), sql("`catalog`.`schema`.`table`"))

  expect_equal(glue_sql2("{.from sql('(SELECT * FROM df_x)')}", .con = con), sql("(SELECT * FROM df_x)"))
})

test_that("glue_sql() interpolates .name correctly", {
  con <- simulate_dbi()
  expect_equal(glue_sql2("{.name 'table'}", .con = con), sql("`table`"))
  expect_equal(glue_sql2("{.name ident('table')}", .con = con), sql("`table`"))
  expect_equal(glue_sql2("{.name ident_q('ta ble')}", .con = con), sql("ta ble"))
})

test_that("glue_sql() interpolates .col correctly", {
  con <- simulate_dbi()
  expect_equal(glue_sql2("{.col 'x'}", .con = con), sql("`x`"))
  expect_equal(glue_sql2("{.col ident('x')}", .con = con), sql("`x`"))
})

test_that("glue_sql() interpolates .kw correctly", {
  withr::local_options(dbplyr_use_colour = TRUE)
  local_reproducible_output(crayon = TRUE)
  withr::local_options(dbplyr_highlight = cli::combine_ansi_styles("blue"))
  con <- simulate_dbi()
  expect_equal(glue_sql2("{.kw 'FROM'}", .con = con), sql("\033[34mFROM\033[39m"))
})

test_that("glue_sql() checks size", {
  con <- simulate_dbi()
  x <- c("a", "b")
  expect_snapshot(error = TRUE, {
    glue_sql2("{.col x}", .con = con)
    glue_sql2("{.col character()}", .con = con)
  })
})

test_that("glue_sql() can collapse", {
  con <- simulate_dbi()
  x <- c("a", "b")
  expect_equal(glue_sql2("{.col x*}", .con = con), sql("`a`, `b`"))
  expect_equal(glue_sql2("{.val x*}", .con = con), sql("'a', 'b'"))

  expect_snapshot(error = TRUE, {
    glue_sql2("{.tbl x*}", .con = con)
    glue_sql2("{.name x*}", .con = con)
    glue_sql2("{.from x*}", .con = con)
  })
})
