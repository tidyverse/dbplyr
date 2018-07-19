context("utils")

test_that("deparse_trunc() expression to text", {
  expect_equal(
    deparse_trunc(expr(test)),
    "test"
  )

  dt <- deparse_trunc(
    expr(!!paste0(rep("x", 200), collapse = ""))
  )
  expect_equal(
    nchar(dt),
    getOption("width")
  )
})

test_that("All objects in the list match the function", {
  expect_true(all_apply(list(1, 2, 3), is.numeric))
  expect_false(all_apply(list(1, "A", 3), is.numeric))
})

test_that("Any object in the list matches the function", {
  expect_true(any_apply(list("A", "B", 1), is.numeric))
  expect_false(any_apply(list("A", "B", "C"), is.numeric))
})

test_that("Last item is dropped", {
  expect_equal(drop_last(c(1, 2)), 1)
})

test_that("NULL for single item vector", {
  expect_null(drop_last(1))
})

test_that("Says 1.1 is not a whole number", {
  expect_false(is.wholenumber(1.1))
})

test_that("Parses the formula correctly", {
  expect_equal(
    deparse_names(expr(x + 1)),
    c("+", "x", "1")
  )
})

test_that("Succesful and not-sucessful commands are identified", {
  expect_true(succeeds("success"))
  expect_false(succeeds(x - 1))
})

test_that("Dots are collapsed into a single variable", {
  expect_equal(
    named_commas(x = 1, y = 2),
    "x = 1, y = 2"
  )
  expect_equal(
    named_commas(1, 2),
    "1, 2"
  )
})

test_that("Correctly identifies the Travis flag", {
  expect_equal(
    in_travis(),
    Sys.getenv("TRAVIS") == "true"
  )
})

test_that("Returns a named vector with correct values", {
  d <- named(x = 1, y = 2)
  expect_equal(
    names(d),
    c("x", "y")
  )
  expect_equal(
    paste(d),
    c("1", "2")
  )
})

test_that("Returns error if no characters are passed", {
  expect_error(c_character(1, 2))
})
