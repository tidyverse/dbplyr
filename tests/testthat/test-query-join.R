context("test-query-join")

test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  qry <- sql_build(left_join(lf1, lf2))

  expect_known_output(print(qry), test_path("test-query-join-print.txt"))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  reg <- list(
    inner = inner_join(lf, lf),
    left = left_join(lf, lf),
    right = right_join(lf, lf),
    full = full_join(lf, lf)
  )
  expect_known_output(print(reg), test_path("sql/join.sql"))
})

test_that("sql_on query doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 3)
  reg <- list(
    inner = inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z"),
    left = left_join(lf1, lf2, sql_on = "LHS.y < RHS.z"),
    right = right_join(lf1, lf2, sql_on = "LHS.y < RHS.z"),
    full = full_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
  )
  expect_known_output(print(reg), test_path("sql/join-on.sql"))
})
