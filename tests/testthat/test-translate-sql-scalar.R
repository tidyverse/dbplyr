test_that("can translate infix expression without parentheses", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(!!expr(2 - 1) * x), sql("(2.0 - 1.0) * `x`"))
  expect_equal(test_translate_sql(!!expr(2 / 1) * x), sql("(2.0 / 1.0) * `x`"))
  expect_equal(test_translate_sql(!!expr(2 * 1) - x), sql("(2.0 * 1.0) - `x`"))
})

test_that("unary minus works with expressions", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(-!!expr(x + 2)), sql("-(`x` + 2.0)"))
  expect_equal(test_translate_sql(--x), sql("--`x`"))
})

test_that("sql_infix generates expected output (#1345)", {
  local_con(simulate_dbi())
  x <- ident_q("x")
  y <- ident_q("y")

  expect_equal(sql_infix("-", pad = FALSE)(x, y), sql("x-y"))
  expect_equal(sql_infix("-", pad = FALSE)(NULL, y), sql("-y"))
  expect_equal(sql_infix("-")(x, y), sql("x - y"))
  expect_equal(sql_infix("-")(NULL, y), sql("- y"))
})

test_that("sql_prefix checks arguments", {
  local_con(simulate_dbi())
  sin_db <- sql_prefix("SIN", 1)

  expect_snapshot(error = TRUE, sin_db(sin(1, 2)))
  expect_snapshot(error = TRUE, sin_db(sin(a = 1)))
})

test_that("runif is translated", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(runif(n())),
    sql("RANDOM()")
  )

  expect_equal(
    test_translate_sql(runif(n(), max = 2)),
    sql("RANDOM() * 2.0")
  )

  expect_equal(
    test_translate_sql(runif(n(), min = 1, max = 2)),
    sql("RANDOM() + 1.0")
  )

  expect_equal(
    test_translate_sql(runif(n(), min = 1, max = 3)),
    sql("RANDOM() * 2.0 + 1.0")
  )

  expect_snapshot(error = TRUE, {
    test_translate_sql(runif(2))
  })
})
