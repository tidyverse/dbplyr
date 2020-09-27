test_that("sql_substr works as expected", {
  local_con(simulate_dbi())
  x <- ident("x")
  substr <- sql_substr("SUBSTR")

  expect_error(substr("test"), 'argument "start" is missing')
  expect_error(substr("test", 0), 'argument "stop" is missing')
  expect_equal(substr("test", 0, 1), sql("SUBSTR('test', 0, 2)"))
  expect_equal(substr("test", 3, 2), sql("SUBSTR('test', 3, 0)"))
  expect_equal(substr("test", 3, 3), sql("SUBSTR('test', 3, 1)"))
})

test_that("sql_str_sub works as expected", {
  local_con(simulate_dbi())
  x <- ident("x")
  str_sub <- sql_str_sub("SUBSTR")

  expect_equal(str_sub(x), sql("SUBSTR(`x`, 1)"))
  expect_equal(str_sub(x, 1), sql("SUBSTR(`x`, 1)"))
  expect_equal(str_sub(x, -1), sql("SUBSTR(`x`, -1)"))
  expect_equal(str_sub(x, 2, 4), sql("SUBSTR(`x`, 2, 3)"))
  expect_equal(str_sub(x, 2, 2), sql("SUBSTR(`x`, 2, 1)"))
  expect_equal(str_sub(x, 2, 0), sql("SUBSTR(`x`, 2, 0)"))
  expect_equal(str_sub(x, 1, -2), sql("SUBSTR(`x`, 1, LENGTH(`x`) - 1)"))
  expect_equal(str_sub(x, 3, -3), sql("SUBSTR(`x`, 3, LENGTH(`x`) - 4)"))
  expect_equal(str_sub(x, -3, 0), sql("SUBSTR(`x`, -3, 0)"))
  expect_equal(str_sub(x, -3, -3), sql("SUBSTR(`x`, -3, 1)"))
})

test_that("str_sub() returns consistent results", {
  mf <- memdb_frame(t = "abcde")

  expect_equal(mf %>% transmute(str_sub(t, -3, -1)) %>% pull(1), "cde")
  expect_equal(mf %>% transmute(str_sub(t, 0, -1)) %>% pull(1), "abcde")
  expect_equal(mf %>% transmute(str_sub(t, 1, -3)) %>% pull(1), "abc")

  expect_equal(mf %>% transmute(str_sub(t, -3, 0)) %>% pull(1), "")
  expect_equal(mf %>% transmute(str_sub(t, 0, 0)) %>% pull(1), "")
  expect_equal(mf %>% transmute(str_sub(t, 1, 0)) %>% pull(1), "")

  expect_equal(mf %>% transmute(str_sub(t, -3, 5)) %>% pull(1), "cde")
  expect_equal(mf %>% transmute(str_sub(t, 0, 1)) %>% pull(1), "a")
  expect_equal(mf %>% transmute(str_sub(t, 1, 3)) %>% pull(1), "abc")
})
