context("translate string helpers")

test_that("sql_substr works as expected", {
  setClass("DummyDBIConnection", representation("DBIConnection"))

  dummy_con <- new("DummyDBIConnection")
  sql_context <- dbplyr:::sql_context
  prev_con <- sql_context$con
  sql_context$con <- dummy_con
  expect_error(sql_substr("SUBSTR")("test"), 'argument "start" is missing')
  expect_error(sql_substr("SUBSTR")("test",0), 'argument "stop" is missing')
  expect_equal(sql_substr("SUBSTR")("test",0,1), sql("SUBSTR('test', 0, 2)"))
  expect_equal(sql_substr("SUBSTR")("test",3,2), sql("SUBSTR('test', 3, 0)"))
  expect_equal(sql_substr("SUBSTR")("test",3,3), sql("SUBSTR('test', 3, 1)"))
  expect_equal(sql_substr("SUBSTRING")("string", 4, 5), sql("SUBSTRING('string', 4, 2)"))

  sql_context$con <- prev_con
})

test_that("sql_str_sub works as expected", {
  setClass("DummyDBIConnection", representation("DBIConnection"))

  dummy_con <- new("DummyDBIConnection")
  sql_context <- dbplyr:::sql_context
  prev_con <- sql_context$con
  sql_context$con <- dummy_con

  expect_equal(sql_str_sub("SUBSTR")("string"), sql("SUBSTR('string', 1)"))
  expect_equal(sql_str_sub("SUBSTRING")("string"), sql("SUBSTRING('string', 1)"))
  expect_equal(sql_str_sub("SUBSTRING")("string", 4, 3), sql("SUBSTRING('string', 4, 0)"))
  expect_equal(sql_str_sub("SUBSTRING")("string", 4, 5), sql("SUBSTRING('string', 4, 2)"))

  # test that the "infinite length" default is respected
  expect_equal(sql_str_sub("SUBSTRING")("string", 4), sql("SUBSTRING('string', 4)"))
  expect_equal(
    sql_str_sub("SUBSTRING", full_length = "compute")("string", 4),
    sql("SUBSTRING('string', 4, LENGTH('string') - 4 + 1)")
    )
  expect_equal(
    sql_str_sub("SUBSTRING", full_length = "compute", compute_method = "LEN")("string", 4),
    sql("SUBSTRING('string', 4, LEN('string') - 4 + 1)")
    )

  sql_context$con <- prev_con
})

