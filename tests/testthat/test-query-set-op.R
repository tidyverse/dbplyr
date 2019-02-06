context("test-query-set-op")

test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  qry <- sql_build(union(lf1, lf2))

  expect_known_output(print(qry), test_path("test-query-set-op-print.txt"))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  reg <- list(
    union = union(lf, lf),
    setdiff = setdiff(lf, lf),
    intersect = intersect(lf, lf)
  )
  expect_known_output(print(reg), test_path("sql/setop.sql"))
})
