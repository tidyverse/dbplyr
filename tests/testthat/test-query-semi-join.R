test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(
    sql_build(semi_join(lf1, lf2 |> filter(z == 2)))
  )
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  expect_snapshot(semi_join(lf, lf))
  expect_snapshot(anti_join(lf, lf))
})
