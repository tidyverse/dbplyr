# Identifiers ------------------------------------------------------------------

ei <- function(...) unclass(escape(ident(c(...)), con = simulate_dbi()))

test_that("identifiers get identifier quoting", {
  expect_equal(ei("x"), '`x`')
})

test_that("identifiers are comma separated", {
  expect_equal(ei("x", "y"), '`x`, `y`')
})

test_that("identifier names become AS", {
  expect_equal(ei(x = "y"), '`y` AS `x`')
})

# Zero-length inputs ------------------------------------------------------

test_that("zero length inputs yield zero length output when not collapsed", {
  con <- simulate_dbi()
  expect_equal(sql_vector(sql(), collapse = NULL, con = con), sql())
  expect_equal(sql_vector(ident(), collapse = NULL, con = con), sql())
})

test_that("zero length inputs yield length-1 output when collapsed", {
  con <- simulate_dbi()

  expect_equal(sql_vector(sql(), parens = FALSE, collapse = "", con = con), sql(""))
  expect_equal(sql_vector(sql(), parens = TRUE, collapse = "", con = con), sql("()"))
  expect_equal(sql_vector(ident(), parens = FALSE, collapse = "", con = con), sql(""))
  expect_equal(sql_vector(ident(), parens = TRUE, collapse = "", con = con), sql("()"))
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
  x1 <- ISOdatetime(2000, 1, 2, 3, 4, 5, tz = "US/Central")
  x2 <- as.Date(x1)
  expect_equal(escape(x1, con = con), sql("'2000-01-02T09:04:05Z'"))
  expect_equal(escape(x2, con = con), sql("'2000-01-02'"))
})

# Raw -----------------------------------------------------------------

test_that("raw is SQL-99 compatible (by default)", {
  con <- simulate_dbi()
  expect_equal(escape(blob::as_blob(raw(0)), con = con), sql("X''"))
  expect_equal(escape(blob::as_blob(as.raw(c(0x01, 0x02, 0x03))), con = con), sql("X'010203'"))
  expect_equal(escape(blob::as_blob(as.raw(c(0x00, 0xff))), con = con), sql("X'00ff'"))
})

# Factor ------------------------------------------------------------------

test_that("factors are translated", {
  con <- simulate_dbi()
  expect_equal(escape(factor(c("a", "b")), con = con), sql("('a', 'b')"))
})

# Helpful errors --------------------------------------------------------

test_that("shiny objects give useful errors", {
  lf <- lazy_frame(a = 1)
  input <- structure(list(), class = "reactivevalues")
  x <- structure(function() "y", class = "reactive")

  expect_snapshot(error = TRUE, lf %>% filter(a == input$x) %>% show_query())
  expect_snapshot(error = TRUE, lf %>% filter(a == x()) %>% show_query())
})

test_that("con must not be NULL", {
  expect_snapshot(error = TRUE, escape("a"))
  expect_snapshot(error = TRUE, sql_vector("a"))
})

test_that("data frames give useful errors", {
  expect_snapshot(error = TRUE, escape(mtcars, con = simulate_dbi()))
})

# names_to_as() -----------------------------------------------------------

test_that("names_to_as() doesn't alias when ident name and value are identical", {
  x <- ident(name = "name")
  y <- sql("`name`")

  expect_equal(names_to_as(y, names2(x),  con = simulate_dbi()),  "`name`")
})

test_that("names_to_as() doesn't alias when ident name is missing", {
  x <- ident("*")
  y <- sql("`*`")

  expect_equal(names_to_as(y, names2(x), con = simulate_dbi()),  "`*`")
})

test_that("names_to_as() aliases when ident name and value are different", {
  x <- ident(new_name = "name")
  y <- sql(new_name = "`name`")

  expect_equal(names_to_as(y, names2(x),  con = simulate_dbi()),  "`name` AS `new_name`")
})
