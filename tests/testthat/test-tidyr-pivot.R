context("pivot_wider")

pw_frame <- memdb_frame(
  x = c("a", "a", "b", "b"),
  y = c("r", "t", "r", "t"),
  z = c(2, 2, 1, 1)
)

test_that("simple case returns expected result", {
  out <- pw_frame %>%
    db_pivot_wider(names_from = y, values_from = z) %>%
    collect()
  expect_equal(out[1, ], tibble(x = "a", r = 2, t = 2))
})

test_that("name_prefix is inserted to column names", {
  out <- pw_frame %>%
    db_pivot_wider(
      names_from = y,
      values_from = z,
      names_prefix = "test_"
    ) %>%
    collect()
  expect_equal(out[1, ], tibble(x = "a", test_r = 2, test_t = 2))
})

test_that("simple case returns expected result", {
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, id_cols = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, names_sep = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, names_repair = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, names_pattern = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, values_fill = "test"))
  expect_error(db_pivot_wider(pw_frame, names_from = y, values_from = z, values_fn = "test"))
})

test_that("all character table returns table as expected", {
  out <- memdb_frame(
    x = c("a", "a", "b", "b"),
    y = c("x", "y", "x", "y"),
    z = c("r", "t", "r", "t")
  ) %>%
    db_pivot_wider(
      names_from = x,
      values_from = z
    ) %>%
    collect()
  expect_equal(out[1, ], tibble(y = "x", a = "r", b = "r"))
})

test_that("empty combinations return NA", {
  out <- memdb_frame(
    x = c("a", "a", "b", "d"),
    y = c("x", "y", "x", "y"),
    z = c("r", "t", "r", "t")
  ) %>%
    db_pivot_wider(
      names_from = x,
      values_from = z
    ) %>%
    collect()
  expect_true(is.na(out[1, 4]))
})
