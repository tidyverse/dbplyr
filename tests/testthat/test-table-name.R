test_that("table_path possess key methods", {
  expect_snapshot({
    name <- table_path(c("x", "y", "z"))
    name
  })

  expect_equal(name[c(1, 3)], table_path(c("x", "z")))
  expect_equal(name[[2]], table_path("y"))
  expect_equal(c(name[[1]], name[[2]]), table_path(c("x", "y")))
})

test_that("can check for table name", {
  foo <- function(y) check_table_path(y)
  expect_snapshot(foo(1), error = TRUE)
})

# as_table_path -----------------------------------------------------------

test_that("can coerce all user facing inputs", {
  con <- simulate_dbi()

  x_esc <- table_path('"x"')
  x_raw <- table_path("x")

  id <- table_path("x")
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), x_raw)

  id <- "x"
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), x_esc)

  id <- I("x")
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), x_raw)

  id <- ident("x")
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), x_esc)

  id <- ident_q("x")
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), x_raw)

  id <- DBI::Id(schema = "foo", table = "bar")
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), table_path('"foo"."bar"'))

  # strip names, simulating DBI 1.2.0
  names(id@name) <- NULL
  expect_equal(as_table_path(id, con), table_path('"foo"."bar"'))

  id <- in_schema("foo", "bar")
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), table_path('"foo"."bar"'))

  id <- in_catalog("foo", "bar", "baz")
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), table_path('"foo"."bar"."baz"'))

  id <- in_catalog("foo", sql("bar"), ident_q("baz"))
  expect_true(is_table_id(id))
  expect_equal(as_table_path(id, con), table_path('"foo".bar.baz'))
})

test_that("vectorised table_path is not a table_id", {
  expect_false(is_table_id(table_path(c("x", "y"))))
})

test_that("strips names", {
  con <- simulate_dbi()
  expect_equal(as_table_path(c(x = "x"), con), table_path('"x"'))

  id <- in_schema(c(x = "a"), "b")
  expect_equal(as_table_path(id, con), table_path('"a"."b"'))

  id <- in_catalog("a", "b", c(x = "c"))
  expect_equal(as_table_path(id, con), table_path('"a"."b"."c"'))
})

test_that("as_table_path validates its inputs", {
  con <- simulate_dbi()
  expect_snapshot(error = TRUE, {
    as_table_path("x")
    as_table_path(c("x", "y"), con)
    as_table_path(1, con)
    as_table_path(I(1), con)
  })
})

test_that("as_table_path warns when using sql", {
  con <- simulate_dbi()
  expect_snapshot(as_table_path(sql("x"), con))
})

# components --------------------------------------------------------------

test_that("can parse components from path", {
  con <- simulate_dbi()

  expect_equal(
    table_path_components(table_path(c("x.y", '"x.y".z', '"x.y"."y.z"')), con),
    list(c("x", "y"), c("x.y", "z"), c("x.y", "y.z"))
  )
})

test_that("can extract names from path", {
  con <- simulate_dbi()

  expect_equal(
    table_path_name(table_path(c("x.y", '"x.y".z', 'x."y.z"')), con),
    c("y", "z", "y.z")
  )
})
