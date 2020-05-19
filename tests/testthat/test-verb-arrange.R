# sql_render --------------------------------------------------------------

test_that("quoting for rendering ordered grouped table", {
  out <- memdb_frame(x = 1, y = 2) %>% group_by(x) %>% arrange(y) %>% ungroup()
  expect_match(out %>% sql_render, "^SELECT [*]\nFROM `[^`]*`\nORDER BY `y`$")
  expect_equal(out %>% collect, tibble(x = 1, y = 2))
})

# sql_build ---------------------------------------------------------------

test_that("arrange skips order_by with order_by override or ignore, with warning", {
  lf <-
    lazy_frame(x = 1, y = 1) %>%
    arrange(x)

  expect_warning(
    out <- sql_build(lf, order_by = "override")
  )
  expect_null(out$order_by)

  expect_warning(
    out <- sql_build(lf, order_by = "erase")
  )
  expect_null(out$order_by)
})

test_that("arrange skips order_by with order_by ignore, silently", {
  lf <-
    lazy_frame(x = 1, y = 1) %>%
    arrange(x)

  out <- sql_build(lf, order_by = "ignore")

  expect_null(out$order_by)
})

test_that("arrange generates order_by by default", {
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

test_that("multiple arranges don't combine, sometimes with warning (#373)", {
  out <- arrange(arrange(lazy_frame(x = 1:3, y = 3:1), x), y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
  expect_warning(sql_render(out))

  out <- arrange(window_order(lazy_frame(x = 1:3, y = 3:1), x), y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
  expect_warning(sql_render(out), NA)

  out <- window_order(arrange(lazy_frame(x = 1:3, y = 3:1), x), y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
  expect_warning(sql_render(out), NA)

  out <- window_order(window_order(lazy_frame(x = 1:3, y = 3:1), x), y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
  expect_warning(sql_render(out), NA)
})

test_that("order is retained after head() (#373)", {
  mf <- lazy_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  out <- mf %>% arrange(y) %>% head(1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
})

test_that("order is lost after verbs, with warning (#373)", {
  mf <- lazy_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  out <- mf %>% arrange(y) %>% select(-y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out))

  out <- mf %>% arrange(y) %>% mutate(y = 1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out))

  out <- mf %>% arrange(y) %>% summarise(x = sum(x))
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out))

  out <- mf %>% arrange(y) %>% filter(x < 1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out))
})
