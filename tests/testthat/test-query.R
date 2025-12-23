test_that("print method renders sql", {
  lf <- lazy_frame(x = 1) |> mutate(y = x + 1)
  expect_snapshot({
    lf$lazy_query
    sql_build(lf)
  })
})
