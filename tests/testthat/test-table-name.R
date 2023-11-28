
# as_table_name -----------------------------------------------------------

test_that("can coerce all user facing inputs", {
  con <- simulate_dbi()

  expect_equal(table_name("`x`"), table_name("`x`"))
  expect_equal(as_table_name("x", con), table_name("`x`"))
  expect_equal(as_table_name(I("x"), con), table_name("x"))
  expect_equal(as_table_name(ident("x"), con), table_name("`x`"))
  expect_equal(as_table_name(ident_q("x"), con), table_name("x"))

  id <- DBI::Id(schema = "foo", table = "bar")
  expect_equal(as_table_name(id, con), table_name("`foo`.`bar`"))
  names(id@name) <- NULL
  expect_equal(as_table_name(id, con), table_name("`foo`.`bar`"))

  expect_equal(
    as_table_name(in_schema("foo", "bar"), con),
    table_name("`foo`.`bar`")
  )
  expect_equal(
    as_table_name(in_catalog("foo", "bar", "baz"), con),
    table_name("`foo`.`bar`.`baz`")
  )
})

test_that("as_table_name validates its inputs", {
  con <- simulate_dbi()
  expect_snapshot(error = TRUE, {
    as_table_name("x")
    as_table_name(1, con)
  })
})

test_that("as_table_name warns when using sql", {
  con <- simulate_dbi()
  expect_snapshot(as_table_name(sql("x"), con))
})
