test_that("sql_substr works as expected", {
  local_con(simulate_dbi())
  x <- ident("x")
  substr <- sql_substr("SUBSTR")

  expect_equal(substr(x, 3, 4), sql("SUBSTR(`x`, 3, 2)"))
  expect_equal(substr(x, 3, 3), sql("SUBSTR(`x`, 3, 1)"))
  expect_equal(substr(x, 3, 2), sql("SUBSTR(`x`, 3, 0)"))
  expect_equal(substr(x, 3, 1), sql("SUBSTR(`x`, 3, 0)"))

  expect_equal(substr(x, 0, 1), sql("SUBSTR(`x`, 1, 1)"))
  expect_equal(substr(x, -1, 1), sql("SUBSTR(`x`, 1, 1)"))

  # Missing arguments
  expect_snapshot(error = TRUE, substr("test"))
  expect_snapshot(error = TRUE, substr("test", 0))

  # Wrong types
  expect_snapshot(error = TRUE, substr("test", "x", 1))
  expect_snapshot(error = TRUE, substr("test", 1, "x"))
})

test_that("substring is also translated", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(substring(x, 3, 4)), sql("SUBSTR(`x`, 3, 2)"))
})

test_that("sql_str_sub works as expected", {
  local_con(simulate_dbi())
  x <- ident("x")
  str_sub <- sql_str_sub("SUBSTR")

  expect_equal(str_sub(x), sql("SUBSTR(`x`, 1)"))
  expect_equal(str_sub(x, 1), sql("SUBSTR(`x`, 1)"))
  expect_equal(str_sub(x, -1), sql("SUBSTR(`x`, LENGTH(`x`))"))
  expect_equal(str_sub(x, 2, 4), sql("SUBSTR(`x`, 2, 3)"))
  expect_equal(str_sub(x, 2, 2), sql("SUBSTR(`x`, 2, 1)"))
  expect_equal(str_sub(x, 2, 0), sql("SUBSTR(`x`, 2, 0)"))
  expect_equal(str_sub(x, 1, -2), sql("SUBSTR(`x`, 1, LENGTH(`x`) - 1)"))
  expect_equal(str_sub(x, 3, -3), sql("SUBSTR(`x`, 3, LENGTH(`x`) - 4)"))
  expect_equal(str_sub(x, -3, 0), sql("SUBSTR(`x`, LENGTH(`x`) - 2, 0)"))
  expect_equal(str_sub(x, -3, -3), sql("SUBSTR(`x`, LENGTH(`x`) - 2, 1)"))
})

test_that("sql_str_sub can require length paramter", {
  local_con(simulate_dbi())
  x <- ident("x")
  str_sub <- sql_str_sub("SUBSTR", optional_length = FALSE)

  expect_equal(str_sub(x), sql("SUBSTR(`x`, 1, LENGTH(`x`))"))
  expect_equal(str_sub(x, 1), sql("SUBSTR(`x`, 1, LENGTH(`x`))"))
  expect_equal(str_sub(x, -1), sql("SUBSTR(`x`, LENGTH(`x`), 1)"))
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

test_that("str_detect(), str_starts(), str_ends() support fixed patterns", {
  mf <- memdb_frame(x = c("%0 start", "end %0", "detect %0 detect", "no", NA))

  # detects fixed pattern
  expect_equal(
    mf %>% transmute(str_starts(x, fixed("%0"))) %>% pull(1),
    c(1, 0, 0, 0, NA)
  )
  # hack to avoid check complaining about not declared imports
  pattern <- rlang::parse_expr("stringr::fixed('%0')")
  expect_equal(
    mf %>% transmute(str_starts(x, !!pattern)) %>% pull(1),
    c(1, 0, 0, 0, NA)
  )

  # also works with ends and detect
  expect_equal(
    mf %>% transmute(str_ends(x, fixed("%0"))) %>% pull(1),
    c(0, 1, 0, 0, NA)
  )
  expect_equal(
    mf %>% transmute(str_detect(x, fixed("%0"))) %>% pull(1),
    c(1, 1, 1, 0, NA)
  )

  # negate works
  expect_equal(
    mf %>% transmute(str_detect(x, fixed("%0"), negate = TRUE)) %>% pull(1),
    c(0, 0, 0, 1, NA)
  )

  expect_error(translate_sql(str_detect(x, "a"), con = simulate_dbi()))
})
