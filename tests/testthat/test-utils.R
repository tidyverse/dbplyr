test_that("Successful and not-successful commands are identified", {
  expect_true(succeeds("success"))
  expect_false(succeeds(x - 1, quiet = TRUE))
})

test_that("Returns error if no characters are passed", {
  expect_error(c_character(1, 2))
})

test_that("n_selects counts SELECT statements", {
  lf <- lazy_frame(x = 1)

  expect_equal(lf |> n_selects(), 1)
  expect_equal(lf |> filter(x > 1) |> n_selects(), 1)
  expect_equal(lf |> summarise(x) |> summarise(x) |> n_selects(), 2)
})
