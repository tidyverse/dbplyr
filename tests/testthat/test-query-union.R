test_that("set ops captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- union(lf1, lf2) |> sql_build()
  expect_s3_class(out, "union_query")
})
