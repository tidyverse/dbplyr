context("test-verb-compute")


# ops ---------------------------------------------------------------------

test_that("preserved across compute and collapse", {
  df1 <- memdb_frame(x = sample(10)) %>% arrange(x)

  df2 <- compute(df1)
  expect_equal(op_sort(df2), list(~x))

  df3 <- collapse(df1)
  expect_equal(op_sort(df3), list(~x))
})

