test_that("sql_substr works as expected", {
  con <- simulate_dbi()

  expect_translation(con, substr(x, 3, 4), "SUBSTR(\"x\", 3, 2)")
  expect_translation(con, substr(x, 3, 3), "SUBSTR(\"x\", 3, 1)")
  expect_translation(con, substr(x, 3, 2), "SUBSTR(\"x\", 3, 0)")
  expect_translation(con, substr(x, 3, 1), "SUBSTR(\"x\", 3, 0)")

  expect_translation(con, substr(x, 0, 1), "SUBSTR(\"x\", 1, 1)")
  expect_translation(con, substr(x, -1, 1), "SUBSTR(\"x\", 1, 1)")

  # Missing arguments
  expect_translation_snapshot(con, substr("test"), error = TRUE)
  expect_translation_snapshot(con, substr("test", 0), error = TRUE)

  # Wrong types
  expect_translation_snapshot(con, substr("test", "x", 1), error = TRUE)
  expect_translation_snapshot(con, substr("test", 1, "x"), error = TRUE)
})

test_that("substring is also translated", {
  con <- simulate_dbi()
  expect_translation(con, substring(x, 3, 4), "SUBSTR(\"x\", 3, 2)")
})

test_that("sql_str_sub works as expected", {
  con <- simulate_dbi()

  expect_translation(con, str_sub(x), "SUBSTR(\"x\", 1)")
  expect_translation(con, str_sub(x, 1), "SUBSTR(\"x\", 1)")
  expect_translation(con, str_sub(x, -1), "SUBSTR(\"x\", LENGTH(\"x\"))")
  expect_translation(con, str_sub(x, 2, 4), "SUBSTR(\"x\", 2, 3)")
  expect_translation(con, str_sub(x, 2, 2), "SUBSTR(\"x\", 2, 1)")
  expect_translation(con, str_sub(x, 2, 0), "SUBSTR(\"x\", 2, 0)")
  expect_translation(
    con,
    str_sub(x, 1, -2),
    "SUBSTR(\"x\", 1, LENGTH(\"x\") - 1)"
  )
  expect_translation(
    con,
    str_sub(x, 3, -3),
    "SUBSTR(\"x\", 3, LENGTH(\"x\") - 4)"
  )
  expect_translation(
    con,
    str_sub(x, -3, 0),
    "SUBSTR(\"x\", LENGTH(\"x\") - 2, 0)"
  )
  expect_translation(
    con,
    str_sub(x, -3, -3),
    "SUBSTR(\"x\", LENGTH(\"x\") - 2, 1)"
  )
})

test_that("sql_str_sub can require length parameter", {
  local_con(simulate_dbi())
  x <- ident("x")
  str_sub <- sql_str_sub("SUBSTR", optional_length = FALSE)

  expect_equal(str_sub(x), sql("SUBSTR(\"x\", 1, LENGTH(\"x\"))"))
  expect_equal(str_sub(x, 1), sql("SUBSTR(\"x\", 1, LENGTH(\"x\"))"))
  expect_equal(str_sub(x, -1), sql("SUBSTR(\"x\", LENGTH(\"x\"), 1)"))
})

test_that("str_sub() returns consistent results", {
  mf <- local_memdb_frame(t = "abcde")

  expect_equal(mf |> transmute(str_sub(t, -3, -1)) |> pull(1), "cde")
  expect_equal(mf |> transmute(str_sub(t, 0, -1)) |> pull(1), "abcde")
  expect_equal(mf |> transmute(str_sub(t, 1, -3)) |> pull(1), "abc")

  expect_equal(mf |> transmute(str_sub(t, -3, 0)) |> pull(1), "")
  expect_equal(mf |> transmute(str_sub(t, 0, 0)) |> pull(1), "")
  expect_equal(mf |> transmute(str_sub(t, 1, 0)) |> pull(1), "")

  expect_equal(mf |> transmute(str_sub(t, -3, 5)) |> pull(1), "cde")
  expect_equal(mf |> transmute(str_sub(t, 0, 1)) |> pull(1), "a")
  expect_equal(mf |> transmute(str_sub(t, 1, 3)) |> pull(1), "abc")
})

test_that("str_detect(), str_starts(), str_ends() support fixed patterns", {
  mf <- local_memdb_frame(
    x = c("%0 start", "end %0", "detect %0 detect", "no", NA)
  )

  # detects fixed pattern
  expect_equal(
    mf |> transmute(str_starts(x, fixed("%0"))) |> pull(1),
    c(1, 0, 0, 0, NA)
  )
  # hack to avoid check complaining about not declared imports
  pattern <- rlang::parse_expr("stringr::fixed('%0')")
  expect_equal(
    mf |> transmute(str_starts(x, !!pattern)) |> pull(1),
    c(1, 0, 0, 0, NA)
  )

  # also works with ends and detect
  expect_equal(
    mf |> transmute(str_ends(x, fixed("%0"))) |> pull(1),
    c(0, 1, 0, 0, NA)
  )
  expect_equal(
    mf |> transmute(str_detect(x, fixed("%0"))) |> pull(1),
    c(1, 1, 1, 0, NA)
  )

  # negate works
  expect_equal(
    mf |> transmute(str_detect(x, fixed("%0"), negate = TRUE)) |> pull(1),
    c(0, 0, 0, 1, NA)
  )

  expect_error(translate_sql(str_detect(x, "a"), con = simulate_dbi()))
})

test_that("basic prefix paste", {
  con <- simulate_dbi()

  expect_translation(con, paste0(x), "CONCAT_WS('', \"x\")")
  expect_translation(con, paste0(x, y), "CONCAT_WS('', \"x\", \"y\")")
  expect_translation(
    con,
    paste0(x, y, sep = " "),
    "CONCAT_WS(' ', \"x\", \"y\")"
  )
})

test_that("basic infix paste", {
  local_con(simulate_dbi())

  paste <- sql_paste_infix("", "&&")
  x <- ident("x")
  y <- ident("y")

  expect_equal(paste(x), sql("CAST(\"x\" AS text)"))
  expect_equal(paste(x, y), sql("\"x\" && \"y\""))
  expect_equal(paste(x, y, sep = " "), sql("\"x\" && ' ' && \"y\""))
})
