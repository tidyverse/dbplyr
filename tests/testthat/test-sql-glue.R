test_that("glue_sql() handles type casting", {
  con <- simulate_dbi()
  x <- "x"

  expect_equal(sql_glue2(con, "{x}"), sql("'x'"))
  expect_equal(sql_glue2(con, "{.id x}", ), sql('"x"'))
  expect_equal(sql_glue2(con, "{.sql x}", ), sql("x"))

  tbl1 <- "table"
  tbl2 <- I("schema.table")
  tbl3 <- in_schema("schema", "table")
  expect_equal(sql_glue2(con, "{.tbl tbl1}"), sql('"table"'))
  expect_equal(sql_glue2(con, "{.tbl tbl2}"), sql("schema.table"))
  expect_equal(sql_glue2(con, "{.tbl tbl3}"), sql('"schema"."table"'))
})

test_that("useful error if bad identifier", {
  con <- simulate_dbi()
  expect_snapshot(error = TRUE, {
    sql_glue2(con, "{.id 1}")
    sql_glue2(con, "{.tbl 1}")
    sql_glue2(con, "{.sql 1}")
  })
})

test_that("glue_sql() can collapse with and without parens", {
  con <- simulate_dbi()
  x <- c("a", "b")

  expect_equal(sql_glue2(con, "{x}"), sql("'a', 'b'"))
  expect_equal(sql_glue2(con, "{x*}"), sql("('a', 'b')"))
})


test_that("glue_sql() can interpolate ...", {
  con <- simulate_dbi()
  f <- function(...) {
    sql_glue2(con, "f({...})")
  }
  expect_equal(f(), sql("f()"))
  expect_equal(f("a"), sql("f('a')"))
  expect_equal(f("a", "b"), sql("f('a', 'b')"))
})

test_that("gives informative errors", {
  con <- simulate_dbi()
  x <- 1
  expect_snapshot(error = TRUE, {
    sql_glue2(con, "{y*}")
    sql_glue2(con, "{1 + }")
    sql_glue2(con, "{.bar x}")
  })
})
