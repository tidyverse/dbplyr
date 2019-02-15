context("translate string helpers")

test_that("sql_substr works as expected", {
  setClass("DummyDBIConnectionSQLSubstr", representation("DBIConnection"))

  dummy_con <- new("DummyDBIConnectionSQLSubstr")
  prev_con <- dbplyr:::set_current_con(dummy_con)

  expect_error(sql_substr("SUBSTR")("test"), 'argument "start" is missing')
  expect_error(sql_substr("SUBSTR")("test",0), 'argument "stop" is missing')
  expect_equal(sql_substr("SUBSTR")("test",0,1), sql("SUBSTR('test', 0, 2)"))
  expect_equal(sql_substr("SUBSTR")("test",3,2), sql("SUBSTR('test', 3, 0)"))
  expect_equal(sql_substr("SUBSTR")("test",3,3), sql("SUBSTR('test', 3, 1)"))
  expect_equal(sql_substr("SUBSTRING")("string", 4, 5), sql("SUBSTRING('string', 4, 2)"))

  dbplyr:::set_current_con(prev_con)
})

test_that("sql_str_sub works as expected", {
  setClass("DummyDBIConnectionSQLStrSub", representation("DBIConnection"))

  dummy_con <- new("DummyDBIConnectionSQLStrSub")
  prev_con <- dbplyr:::set_current_con(dummy_con)

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

  dbplyr:::set_current_con(prev_con)
})

