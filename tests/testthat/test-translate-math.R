context("translate-math")

test_that("basic arithmetic is correct", {
  expect_equal(translate_sql(1 + 2), sql("1.0 + 2.0"))
  expect_equal(translate_sql(2 * 4), sql("2.0 * 4.0"))
  expect_equal(translate_sql(5 ^ 2), sql("POWER(5.0, 2.0)"))
  expect_equal(translate_sql(100L %% 3L), sql("100 % 3"))
})

test_that("small numbers aren't converted to 0", {
  expect_equal(translate_sql(1e-9), sql("1e-09"))
})

# minus -------------------------------------------------------------------

test_that("unary minus flips sign of number", {
  expect_equal(translate_sql(-10L), sql("-10"))
  expect_equal(translate_sql(x == -10), sql('"x" = -10.0'))
  expect_equal(translate_sql(x %in% c(-1L, 0L)), sql('"x" IN (-1, 0)'))
})

test_that("unary minus wraps non-numeric expressions", {
  expect_equal(translate_sql(-(1L + 2L)), sql("-(1 + 2)"))
  expect_equal(translate_sql(-mean(x, na.rm = TRUE), window = FALSE), sql('-AVG("x")'))
})

test_that("binary minus subtracts", {
  expect_equal(translate_sql(1L - 10L), sql("1 - 10"))
})

# log ---------------------------------------------------------------------

test_that("log base comes first", {
  expect_equal(translate_sql(log(x, 10)), sql('LOG(10.0, "x")'))
})

test_that("log becomes ln", {
  expect_equal(translate_sql(log(x)), sql('LN("x")'))
})

test_that("sqlite mimics two argument log", {
  translate_sqlite <- function(...) {
    translate_sql(..., con = src_memdb()$con)
  }

  expect_equal(translate_sqlite(log(x)), sql('LOG(`x`)'))
  expect_equal(translate_sqlite(log(x, 10)), sql('LOG(`x`) / LOG(10.0)'))
})

test_that("postgres mimics two argument log", {
  translate_postgres <- function(...) {
    translate_sql(..., con = simulate_postgres())
  }

  expect_equal(translate_postgres(log(x)), sql('LN(\"x\")'))
  expect_equal(translate_postgres(log(x, 10)), sql('LOG(\"x\") / LOG(10.0)'))
  expect_equal(translate_postgres(log(x, 10L)), sql('LOG(\"x\") / LOG(10)'))
})
