context("pivot_longer")

test_that("simple case returns expected result", {
  out <- memdb_frame(x = 1, y = 1) %>%
    db_pivot_longer(-x) %>%
    collect()
  expect_equal(out, tibble(x = 1, name = "y", value = 1))
})

test_that("name_prefix argument removes first part of name", {
  out <- memdb_frame(test_x = 1, test_y = 1) %>%
    db_pivot_longer(-test_x, names_prefix = "test_") %>%
    collect()
  expect_equal(out, tibble(test_x = 1, name = "y", value = 1))
})

test_that("simple case returns expected result", {
  out <- memdb_frame(x = 1, y = 1)
  expect_error(db_pivot_longer(out, -x, names_sep = "test"))
  expect_error(db_pivot_longer(out, -x, names_pattern = "test"))
  expect_error(db_pivot_longer(out, -x, names_ptypes =  "test"))
})
