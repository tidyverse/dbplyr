test_that("default extracts last var from data frame", {
  df <- local_memdb_frame(x = 1:3, z = 4:6)
  expect_equal(pull(df), 4:6)
})

test_that("can extract by name, or positive/negative position", {
  x <- 1:3
  df <- local_memdb_frame(x = x, y = 4:6)

  expect_equal(pull(df, x), x)
  expect_equal(pull(df, 1L), x)
  expect_equal(pull(df, 1), x)
  expect_equal(pull(df, -2), x)
  expect_equal(pull(df, -2L), x)
})

test_that("extracts correct column from grouped tbl", {
  mf <- local_memdb_frame(id = "a", value = 42)
  gf <- mf |> group_by(id)

  expect_equal(pull(mf, value), 42)
})

test_that("doesn't unnecessarily select", {
  mf <- local_memdb_frame(x = c(3, 1, 2))
  # no warning about select after arrange
  expect_warning(out <- mf |> arrange(x) |> pull(), NA)
  expect_equal(out, 1:3)
})

test_that("can extract named vectors", {
  x <- 1:3
  y <- letters[x]
  df <- local_memdb_frame(x = x, y = y)
  xn <- set_names(x, y)

  expect_equal(pull(df, x), x)
  expect_equal(pull(df, x, y), xn)
  expect_equal(pull(df, 1, 2), xn)
  expect_equal(names(pull(df, x, y)), y)
})


test_that("ungroup() produces nice error messages", {
  expect_snapshot(error = TRUE, {
    local_memdb_frame("df", x = 1) |> pull(non_existent)
    local_memdb_frame("df", x = 1) |> pull("non_existent")
    local_memdb_frame("df", x = 1) |> pull(1000)

    local_memdb_frame("df", x = 1) |> pull(x, "name_non_existent")
  })
})
