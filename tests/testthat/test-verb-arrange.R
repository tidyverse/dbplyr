context("arrange")

test_that("two arranges equivalent to one", {
  mf <- memdb_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  mf1 <- mf %>% arrange(x, y)
  mf2 <- mf %>% arrange(y) %>% arrange(x)

  expect_equal_tbl(mf1, mf2)
})


# sql_build ---------------------------------------------------------------

test_that("arrange generates order_by", {
  out <- lazy_frame(x = 1, y = 1) %>%
    arrange(x) %>%
    sql_build()

  expect_equal(out$order_by, sql('`x`'))
})

test_that("arrange converts desc", {
  out <- lazy_frame(x = 1, y = 1) %>%
    arrange(desc(x)) %>%
    sql_build()

  expect_equal(out$order_by, sql('`x` DESC'))
})

test_that("grouped arrange doesn't order by groups", {
  out <- lazy_frame(x = 1, y = 1) %>%
    group_by(x) %>%
    arrange(y) %>%
    sql_build()

  expect_equal(out$order_by, sql('`y`'))
})


# ops ---------------------------------------------------------------------

test_that("arranges captures DESC", {
  out <- lazy_frame(x = 1:3, y = 3:1) %>% arrange(desc(x))

  expect_equal(op_sort(out), list(~desc(x)))
})

test_that("multiple arranges combine", {
  out <- lazy_frame(x = 1:3, y = 3:1) %>% arrange(x) %>% arrange(y)
  out <- arrange(arrange(lazy_frame(x = 1:3, y = 3:1), x), y)

  expect_equal(op_sort(out), list(~x, ~y))
})

