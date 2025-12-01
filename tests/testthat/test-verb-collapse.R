test_that("sorting preserved across collapse", {
  df1 <- memdb_frame(x = sample(10)) |> arrange(x)

  df3 <- collapse(df1)
  expect_equal(get_expr(op_sort(df3)[[1]]), quote(x))
})
