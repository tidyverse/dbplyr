context("copy_to")

test_that("can round trip basic data frame", {
  df <- test_frame(x = c(1, 10, 9, NA), y = letters[1:4])
  expect_equal_tbls(df)
})

test_that("NAs in character fields handled by db sources (#2256)", {
  df <- test_frame(
    x = c("a", "aa", NA),
    y = c(NA, "b", "bb"),
    z = c("cc", NA, "c")
  )
  expect_equal_tbls(df)
})

test_that("src_sql allows you to overwrite", {
  name <- random_table_name()
  copy_to(src_memdb(), tibble(x = 1), name = name)

  # Can't check for specific error messages because will vary
  expect_error(copy_to(src_memdb(), tibble(x = 1), name = name))

  df2 <- tibble(x = 2)
  copy_to(src_memdb(), df2, name = name, overwrite = TRUE)
  expect_equal(collect(tbl(src_memdb(), name)), df2)
})

