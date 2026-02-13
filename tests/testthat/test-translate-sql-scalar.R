test_that("can translate infix expression without parentheses", {
  con <- dialect_ansi()
  expect_translation(con, !!expr(2 - 1) * x, '(2.0 - 1.0) * "x"')
  expect_translation(con, !!expr(2 / 1) * x, '(2.0 / 1.0) * "x"')
  expect_translation(con, !!expr(2 * 1) - x, '(2.0 * 1.0) - "x"')
})

test_that("unary minus works with expressions", {
  con <- dialect_ansi()
  expect_translation(con, -!!expr(x + 2), '-("x" + 2.0)')
  expect_translation(con, --x, '--"x"')
})

test_that("sql_infix generates expected output (#1345)", {
  local_con(dialect_ansi())
  x <- ident_q("x")
  y <- ident_q("y")

  expect_equal(sql_infix("-", pad = FALSE)(x, y), sql("x-y"))
  expect_equal(sql_infix("-", pad = FALSE)(NULL, y), sql("-y"))
  expect_equal(sql_infix("-")(x, y), sql("x - y"))
  expect_equal(sql_infix("-")(NULL, y), sql("- y"))
})

test_that("sql_prefix checks arguments", {
  con <- dialect_ansi()
  sin_db <- sql_prefix("SIN", 1)

  expect_snapshot(error = TRUE, sin_db(sin(1, 2)))
  expect_snapshot(error = TRUE, sin_db(sin(a = 1)))
})

test_that("runif is translated", {
  con <- dialect_ansi()
  expect_translation(con, runif(n()), "RANDOM()")
  expect_translation(con, runif(n(), max = 2), "RANDOM() * 2.0")
  expect_translation(con, runif(n(), min = 1, max = 2), "RANDOM() + 1.0")
  expect_translation(con, runif(n(), min = 1, max = 3), "RANDOM() * 2.0 + 1.0")

  expect_translation_snapshot(con, runif(2), error = TRUE)
})

test_that("sql_runif() still works with an expression", {
  local_con(dialect_ansi())

  expect_equal(sql_runif(RANDOM()), sql("RANDOM()"))
  expect_equal(sql_runif(RANDOM(), min = 1, max = 2), sql("RANDOM() + 1.0"))
})
