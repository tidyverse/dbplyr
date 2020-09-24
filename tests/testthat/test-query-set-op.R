test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(sql_build(union(lf1, lf2)))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  expect_snapshot(union(lf, lf))
  expect_snapshot(setdiff(lf, lf))
  expect_snapshot(intersect(lf, lf))

  expect_snapshot(union(lf, lf, all = TRUE))
  expect_snapshot(setdiff(lf, lf, all = TRUE))
  expect_snapshot(intersect(lf, lf, all = TRUE))
})
