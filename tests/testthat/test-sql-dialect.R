test_that("new_sql_dialect() creates subclass", {
  d <- new_sql_dialect(
    "test",
    quote_identifier = function(x) sql_quote(x, "`")
  )
  expect_s3_class(d, c("sql_dialect_test", "sql_dialect"))
})

test_that("new_sql_dialect() validates inputs", {
  expect_snapshot(error = TRUE, {
    new_sql_dialect(1)
    new_sql_dialect("x", quote_identifier = "a function")
    new_sql_dialect("x", quote_identifier = \(x) x, has_window_clause = "yes")
  })
})

test_that("sql_dialect() is idempotent for dialects and connections", {
  d <- new_sql_dialect("test", quote_identifier = identity)
  expect_identical(sql_dialect(d), d)

  # for backward compatibility
  con <- dialect_ansi()
  expect_identical(sql_dialect(con), con)
})

test_that("sql_has_window_clause() returns correct values", {
  d1 <- new_sql_dialect(
    "test",
    quote_identifier = identity,
    has_window_clause = FALSE
  )
  d2 <- new_sql_dialect(
    "test",
    quote_identifier = identity,
    has_window_clause = TRUE
  )

  expect_false(sql_has_window_clause(d1))
  expect_true(sql_has_window_clause(d2))

  # DBIConnection defaults to FALSE
  expect_false(sql_has_window_clause(dialect_ansi()))
})

test_that("sql_has_table_alias_with_as() returns correct values", {
  d1 <- new_sql_dialect(
    "test",
    quote_identifier = identity,
    has_table_alias_with_as = TRUE
  )
  d2 <- new_sql_dialect(
    "test",
    quote_identifier = identity,
    has_table_alias_with_as = FALSE
  )

  expect_true(sql_has_table_alias_with_as(d1))
  expect_false(sql_has_table_alias_with_as(d2))

  # DBIConnection defaults to TRUE
  expect_true(sql_has_table_alias_with_as(dialect_ansi()))
})

test_that("print method shows class", {
  d <- new_sql_dialect("test", quote_identifier = identity)
  expect_snapshot(d)
})
