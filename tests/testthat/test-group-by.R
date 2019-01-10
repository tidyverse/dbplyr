context("group_by")

test_that("group_by with add = TRUE adds groups", {
  mf <- memdb_frame(x = 1:3, y = 1:3)
  gf1 <- mf %>% group_by(x, y)
  gf2 <- mf %>% group_by(x) %>% group_by(y, add = TRUE)

  expect_equal(group_vars(gf1), c("x", "y"))
  expect_equal(group_vars(gf2), c("x", "y"))
})

test_that("collect, collapse and compute preserve grouping", {
  g <- memdb_frame(x = 1:3, y = 1:3) %>% group_by(x, y)

  expect_equal(group_vars(compute(g)), c("x", "y"))
  expect_equal(group_vars(collapse(g)), c("x", "y"))
  expect_equal(group_vars(collect(g)), c("x", "y"))
})

test_that("joins preserve grouping", {
  g <- memdb_frame(x = 1:3, y = 1:3) %>% group_by(x)

  expect_equal(group_vars(inner_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(left_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(semi_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(anti_join(g, g, by = c("x", "y"))), "x")
})

test_that("group_by can perform mutate", {
  mf <- memdb_frame(x = 3:1, y = 1:3)
  out <- mf %>%
    group_by(z = x + y) %>%
    summarise(n = n()) %>%
    collect()

  expect_equal(out, tibble(z = 4L, n = 3L))
})
