# Identifiers ------------------------------------------------------------------

ei <- function(...) unclass(escape(ident(c(...)), con = simulate_dbi()))

test_that("identifiers get identifier quoting", {
  expect_equal(ei("x"), '"x"')
})

test_that("identifiers are comma separated", {
  expect_equal(ei("x", "y"), '"x", "y"')
})

test_that("identifier names become AS", {
  expect_equal(ei(x = "y"), '"y" AS "x"')
})

# sql_collapse ------------------------------------------------------------------

test_that("sql_collapse collapses with separator", {
  expect_equal(sql_collapse(sql("a", "b")), sql("a b"))
  expect_equal(sql_collapse(sql("a", "b"), collapse = ", "), sql("a, b"))
})

test_that("sql_collapse handles 0-length inputs", {
  expect_equal(sql_collapse(character()), sql(""))
  expect_equal(sql_collapse(character(), collapse = NULL), sql())

  expect_equal(sql_collapse(character(), parens = TRUE), sql("()"))
  expect_equal(sql_collapse(character(), collapse = NULL, parens = TRUE), sql())
})

test_that("sql_collapse controls parens", {
  expect_equal(sql_collapse(sql("a")), sql("a"))
  expect_equal(sql_collapse(sql("a", "b")), sql("a b"))

  expect_equal(sql_collapse(sql("a"), parens = TRUE), sql("(a)"))
  expect_equal(sql_collapse(sql("a", "b"), parens = TRUE), sql("(a b)"))
})

# Numeric ------------------------------------------------------------------

test_that("missing values become null", {
  con <- simulate_dbi()

  expect_equal(escape(NA, con = con), sql("NULL"))
  expect_equal(escape(NA_real_, con = con), sql("NULL"))
  expect_equal(escape(NA_integer_, con = con), sql("NULL"))
  expect_equal(escape(NA_character_, con = con), sql("NULL"))
})

test_that("-Inf and Inf are expanded and quoted", {
  con <- simulate_dbi()
  expect_equal(escape(Inf, con = con), sql("'Infinity'"))
  expect_equal(escape(-Inf, con = con), sql("'-Infinity'"))
})

test_that("can escape integer64 values", {
  con <- simulate_dbi()
  skip_if_not_installed("bit64")

  expect_equal(
    escape(bit64::as.integer64(NA), con = con),
    sql("NULL")
  )
  expect_equal(
    escape(bit64::as.integer64("123456789123456789"), con = con),
    sql("123456789123456789")
  )
})

test_that("recognises integerish numerics", {
  expect_equal(is_whole_number(c(1.1, 1.0, 1.000001)), c(FALSE, TRUE, FALSE))
  con <- simulate_dbi()
  expect_equal(
    escape(c(1.1, 1, 1.000001), con = con),
    sql("(1.1, 1.0, 1.000001)")
  )
})

# Logical -----------------------------------------------------------------

test_that("logical is SQL-99 compatible (by default)", {
  con <- simulate_dbi()
  expect_equal(escape(TRUE, con = con), sql("TRUE"))
  expect_equal(escape(FALSE, con = con), sql("FALSE"))
  expect_equal(escape(NA, con = con), sql("NULL"))
})

# Date-time ---------------------------------------------------------------

test_that("date and date-times are converted to ISO 8601", {
  con <- simulate_dbi()
  x1 <- ISOdatetime(2000, 1, 2, 3, 4, 5, tz = "America/New_York")
  x2 <- as.Date(x1)
  expect_equal(escape(x1, con = con), sql("'2000-01-02T08:04:05Z'"))
  expect_equal(escape(x2, con = con), sql("'2000-01-02'"))
})

# Raw -----------------------------------------------------------------

test_that("raw is SQL-99 compatible (by default)", {
  con <- simulate_dbi()
  expect_equal(escape(blob::as_blob(raw(0)), con = con), sql("X''"))
  expect_equal(
    escape(blob::as_blob(as.raw(c(0x01, 0x02, 0x03))), con = con),
    sql("X'010203'")
  )
  expect_equal(
    escape(blob::as_blob(as.raw(c(0x00, 0xff))), con = con),
    sql("X'00ff'")
  )
})

test_that("can translate raw to blob spec", {
  con <- simulate_dbi()

  expect_equal(sql_escape_raw(con, NULL), "NULL")
  expect_equal(sql_escape_raw(con, charToRaw("abc")), "X'616263'")
})

# Factor ------------------------------------------------------------------

test_that("factors are translated", {
  con <- simulate_dbi()
  expect_equal(escape(factor(c("a", "b")), con = con), sql("('a', 'b')"))
})

# Helpful errors --------------------------------------------------------

test_that("con must not be NULL", {
  expect_snapshot(error = TRUE, escape("a"))
  expect_snapshot(error = TRUE, sql_vector("a"))
})

test_that("other objects get informative error", {
  lf <- lazy_frame(x = 1)

  input <- structure(list(), class = "reactivevalues")
  x <- structure(function() "y", class = "reactive")
  df <- data.frame(x = 1)

  expect_snapshot(
    {
      lf |> filter(x == input)
      lf |> filter(x == x())
      lf |> filter(x == df)
      lf |> filter(x == mean)
    },
    error = TRUE
  )
})

# names_to_as() -----------------------------------------------------------

test_that("names_to_as() correctly aliases", {
  con <- simulate_dbi()

  # no alias when name is missing
  expect_equal(names_to_as(con, c("name")), sql("name"))
  # no alias when (quoted) name and value are identical
  expect_equal(names_to_as(con, c(name = '"name"')), sql('"name"'))
  # alias when name and value are different
  expect_equal(names_to_as(con, c(new = "old")), sql('old AS "new"'))
})
