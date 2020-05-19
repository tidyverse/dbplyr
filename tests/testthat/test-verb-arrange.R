context("arrange")

test_that("second arrange overrides first", {
  mf <- memdb_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  mf1 <- mf %>% arrange(x)

  expect_warning(
    mf2 <- mf %>% arrange(y) %>% arrange(x)
  )

  expect_equal_tbl(mf1, mf2)
})

# sql_render --------------------------------------------------------------

test_that("quoting for rendering ordered grouped table", {
  out <- memdb_frame(x = 1, y = 2) %>% group_by(x) %>% arrange(y) %>% ungroup()
  expect_match(out %>% sql_render, "^SELECT [*]\nFROM `[^`]*`\nORDER BY `y`$")
  expect_equal(out %>% collect, tibble(x = 1, y = 2))
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

test_that("grouped arrange order by groups when .by_group  is set to TRUE", {
  lf <- lazy_frame(x = 1, y = 1, con = simulate_dbi())
  out <- lf %>%
    group_by(x) %>%
    arrange(y, .by_group = TRUE) %>%
    sql_build()
  expect_equal(out$order_by, sql(c('`x`','`y`')))
})

# ops ---------------------------------------------------------------------

test_that("arranges captures DESC", {
  out <- lazy_frame(x = 1:3, y = 3:1) %>% arrange(desc(x))

  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(desc(x))))
})

test_that("multiple arranges don't combine (#373)", {
  expect_warning(
    out <- arrange(arrange(lazy_frame(x = 1:3, y = 3:1), x), y)
  )
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))

  expect_warning(
    out <- arrange(window_order(lazy_frame(x = 1:3, y = 3:1), x), y)
  )
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))

  expect_warning(
    out <- window_order(arrange(lazy_frame(x = 1:3, y = 3:1), x), y)
  )
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))

  expect_warning(
    out <- window_order(window_order(lazy_frame(x = 1:3, y = 3:1), x), y)
  )
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
})
