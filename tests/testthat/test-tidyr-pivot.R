context("pivot_wider")

pw_frame <- memdb_frame(
  x = c("a", "a", "b", "b"),
  y = c("x", "y", "x", "y"),
  z = c(2, 2, 1, 1)
  )

test_that("simple case returns expected result", {
  out <- pw_frame %>%
    db_pivot_wider(names_from = y, values_from = z)  %>%
    collect()
  expect_equal(out[1, ], tibble(x = "a", z_x = 2, z_y = 2 ))
})

test_that("name_prefix is inserted to column names", {
  out <- pw_frame %>%
    db_pivot_wider(
      names_from = y,
      values_from = z,
      names_prefix = "test_"
      )  %>%
    collect()
  expect_equal(out[1, ], tibble(x = "a", z_test_x = 2, z_test_y = 2 ))
})

test_that("simple case returns expected result", {
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, id_cols = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, names_sep = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, names_repair = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, names_pattern = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, values_fill = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, values_fn = "test"))
})
