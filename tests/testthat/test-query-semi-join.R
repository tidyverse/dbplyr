test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(sql_build(semi_join(lf1, lf2)))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  expect_snapshot(semi_join(lf, lf))
  expect_snapshot(anti_join(lf, lf))
})

test_that("CTEs work with join", {
  lf1 <- lazy_frame(x = 1, y = 2, .name = "df1")
  lf2 <- lazy_frame(x = 1, z = 3, .name = "df2")
  # no CTE as no CTE required
  expect_snapshot(semi_join(lf1, lf2, by = "x") %>% remote_query(cte = TRUE))

  lf1_f <- lf1 %>% filter(x == 1)
  lf2_f <- lf2 %>% filter(x == 1)
  expect_snapshot(semi_join(lf1_f, lf2_f, by = "x") %>% remote_query(cte = TRUE))

  # CTE is reused
  expect_snapshot(semi_join(lf1_f, lf1_f, by = "x") %>% remote_query(cte = TRUE))
})
