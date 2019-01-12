context("test-sql-build.R")


test_that("connection affects SQL generation", {
  lf <- lazy_frame(x = 1, y = 5) %>% summarise(n = n())

  out1 <- lf %>% sql_build()
  out2 <- lf %>% sql_build(con = simulate_postgres())

  expect_equal(out1$select, sql('COUNT() AS "n"'))
  expect_equal(out2$select, sql('COUNT(*) AS \"n\"'))
})
