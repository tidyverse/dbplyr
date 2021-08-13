test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(sql_build(left_join(lf1, lf2)))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(inner_join(lf, lf))
  expect_snapshot(left_join(lf, lf))
  expect_snapshot(right_join(lf, lf))
  expect_snapshot(full_join(lf, lf))
})

test_that("only disambiguates shared variables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(left_join(lf1, lf2))
  expect_snapshot(left_join(lf1, lf2, by = c("y" = "z")))
})

test_that("sql_on query doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 3)

  expect_snapshot(inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(left_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(right_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(full_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(semi_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(anti_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
})

test_that("CTEs work with join", {
  lf1 <- lazy_frame(x = 1, y = 2, .name = "df1")
  lf2 <- lazy_frame(x = 1, z = 3, .name = "df2")
  # no CTE as no CTE required
  expect_snapshot(inner_join(lf1, lf2, by = "x") %>% remote_query(cte = TRUE))

  lf1_f <- lf1 %>% filter(x == 1)
  lf2_f <- lf2 %>% filter(x == 1)
  expect_snapshot(inner_join(lf1_f, lf2_f, by = "x") %>% remote_query(cte = TRUE))

  # CTE is reused
  expect_snapshot(inner_join(lf1_f, lf1_f, by = "x") %>% remote_query(cte = TRUE))
})
