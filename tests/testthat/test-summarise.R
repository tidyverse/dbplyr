context("Summarise")

test_that("summarise peels off a single layer of grouping", {
  mf1 <- memdb_frame(x = 1, y = 1, z = 2) %>% group_by(x, y)
  mf2 <- mf1 %>% summarise(n = n())
  expect_equal(group_vars(mf2), "x")

  mf3 <- mf2 %>% summarise(n = n())
  expect_equal(group_vars(mf3), character())
})

test_that("summarise performs partial evaluation", {
  mf1 <- memdb_frame(x = 1)

  val <- 1
  mf2 <- mf1 %>% summarise(y = x == val) %>% collect()

  expect_equal(mf2$y, 1)
})
