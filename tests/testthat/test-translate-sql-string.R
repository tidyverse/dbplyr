context("translate string helpers")

test_that("sql_substr works as expected", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))
  x <- ident("x")
  substr <- sql_substr("SUBSTR")

  expect_error(substr("test"), 'argument "start" is missing')
  expect_error(substr("test", 0), 'argument "stop" is missing')
  expect_equal(substr("test", 0, 1), sql("SUBSTR('test', 0, 2)"))
  expect_equal(substr("test", 3, 2), sql("SUBSTR('test', 3, 0)"))
  expect_equal(substr("test", 3, 3), sql("SUBSTR('test', 3, 1)"))
})

test_that("sql_str_sub works as expected", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))
  x <- ident("x")
  substr <- sql_str_sub("SUBSTR")

  expect_equal(substr(x), sql("SUBSTR(`x`, 1)"))
  expect_equal(substr(x, 1), sql("SUBSTR(`x`, 1)"))
  expect_equal(substr(x, -1), sql("SUBSTR(`x`, -1)"))
  expect_equal(substr(x, 2, 4), sql("SUBSTR(`x`, 2, 3)"))
  expect_equal(substr(x, 2, 2), sql("SUBSTR(`x`, 2, 1)"))
  expect_equal(substr(x, 1, -2), sql("SUBSTR(`x`, 1, LENGTH(`x`) - 1)"))
  expect_equal(substr(x, -3, -3), sql("SUBSTR(`x`, -3, 1)"))
})
