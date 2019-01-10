context("arrange")

test_that("two arranges equivalent to one", {
  mf <- memdb_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  mf1 <- mf %>% arrange(x, y)
  mf2 <- mf %>% arrange(y) %>% arrange(x)

  expect_equal_tbl(mf1, mf2)
})
