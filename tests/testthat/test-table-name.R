
# as_table_name -----------------------------------------------------------

test_that("can coerce all user facing inputs", {
  con <- simulate_dbi()

  x_esc <- table_name("`x`")
  x_raw <- table_name("x")

  id <- table_name("x")
  expect_true(is_table_id(id))
  expect_equal(id, x_raw)

  id <- "x"
  expect_true(is_table_id(id))
  expect_equal(as_table_name(id, con), x_esc)

  id <- I("x")
  expect_true(is_table_id(id))
  expect_equal(as_table_name(id, con), x_raw)

  id <- ident("x")
  expect_true(is_table_id(id))
  expect_equal(as_table_name(id, con), x_esc)

  id <- ident_q("x")
  expect_true(is_table_id(id))
  expect_equal(as_table_name(id, con), x_raw)

  id <- DBI::Id(schema = "foo", table = "bar")
  expect_true(is_table_id(id))
  expect_equal(as_table_name(id, con), table_name("`foo`.`bar`"))

  # strip names, simulating DBI 1.2.0
  names(id@name) <- NULL
  expect_equal(as_table_name(id, con), table_name("`foo`.`bar`"))

  id <- in_schema("foo", "bar")
  expect_true(is_table_id(id))
  expect_equal(as_table_name(id, con), table_name("`foo`.`bar`"))

  id <- in_catalog("foo", "bar", "baz")
  expect_true(is_table_id(id))
  expect_equal(as_table_name(id, con), table_name("`foo`.`bar`.`baz`"))
})

test_that("as_table_name validates its inputs", {
  con <- simulate_dbi()
  expect_snapshot(error = TRUE, {
    as_table_name("x")
    as_table_name(c("x", "y"), con)
    as_table_name(1, con)
    as_table_name(I(1), con)
  })
})

test_that("as_table_name warns when using sql", {
  con <- simulate_dbi()
  expect_snapshot(as_table_name(sql("x"), con))
})
