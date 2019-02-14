context("translate string helpers")

test_that("sql_substr works as expected", {
  setClass("DummyDBIConnection", representation("DBIConnection"))

  dummy_con <- new("DummyDBIConnection")
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
  prev_con <- sql_context$con
  sql_context$con <- dummy_con

  sql_str_sub()("string")

  sql_context$con <- prev_con
})
