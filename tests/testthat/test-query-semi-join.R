context("test-query-semi-join")

test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  qry <- sql_build(semi_join(lf1, lf2))

  expect_known_output(print(qry), test_path("test-query-semi-join-print.txt"))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  reg <- list(
    semi = semi_join(lf, lf),
    anti = anti_join(lf, lf)
  )
  expect_known_output(print(reg), test_path("sql/semi-join.sql"))
})
