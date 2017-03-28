context("copy_to")

test_that("src_sql allows you to overwrite", {
  name <- random_table_name()
  copy_to(src_memdb(), tibble(x = 1), name = name)

  # Can't check for specific error messages because will vary
  expect_error(copy_to(src_memdb(), tibble(x = 1), name = name))

  df2 <- tibble(x = 2)
  copy_to(src_memdb(), df2, name = name, overwrite = TRUE)
  expect_equal(collect(tbl(src_memdb(), name)), df2)
})


