test_that("Succesful and not-sucessful commands are identified", {
  expect_true(succeeds("success"))
  expect_false(succeeds(x - 1, quiet = TRUE))
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

test_that("Returns error if no characters are passed", {
  expect_error(c_character(1, 2))
})
