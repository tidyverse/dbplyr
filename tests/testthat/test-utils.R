test_that("Successful and not-successful commands are identified", {
  expect_true(succeeds("success"))
  expect_false(succeeds(x - 1, quiet = TRUE))
})

test_that("Returns error if no characters are passed", {
  expect_error(c_character(1, 2))
})
