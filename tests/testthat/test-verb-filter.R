context("filter")

test_that("filter captures local variables", {
  mf <- memdb_frame(x = 1:5, y = 5:1)

  z <- 3
  df1 <- mf %>% filter(x > z) %>% collect()
  df2 <- mf %>% collect() %>% filter(x > z)

  expect_equal_tbl(df1, df2)
})

test_that("two filters equivalent to one", {
  mf <- memdb_frame(x = 1:5, y = 5:1)

  df1 <- mf %>% filter(x > 3) %>% filter(y < 3)
  df2 <- mf %>% filter(x > 3, y < 3)
  expect_equal_tbl(df1, df2)
})


test_that("each argument gets implicit parens", {
  mf <- memdb_frame(
    v1 = c("a", "b", "a", "b"),
    v2 = c("b", "a", "a", "b"),
    v3 = c("a", "b", "c", "d")
  )

  mf1 <- mf %>% filter((v1 == "a" | v2 == "a") & v3 == "a")
  mf2 <- mf %>% filter(v1 == "a" | v2 == "a", v3 == "a")
  expect_equal_tbl(mf1, mf2)
})


# SQL generation --------------------------------------------------------

test_that("filter calls windowed versions of sql functions", {
  df1 <- memdb_frame(x = 1:10, g = rep(c(1, 2), each = 5))

  out <- df1 %>% group_by(g) %>% filter(dplyr::row_number(x) < 3) %>% collect()
  expect_equal(out$x, c(1L, 2L, 6L, 7L))
})

test_that("recycled aggregates generate window function", {
  df1 <- memdb_frame(x = 1:10, g = rep(c(1, 2), each = 5))

  out <- df1 %>%
    group_by(g) %>%
    filter(x > mean(x, na.rm = TRUE)) %>%
    collect()
  expect_equal(out$x, c(4L, 5L, 9L, 10L))
})

test_that("cumulative aggregates generate window function", {
  df1 <- memdb_frame(x = c(1:3, 2:4), g = rep(c(1, 2), each = 3))
  out <- df1 %>%
    group_by(g) %>%
    arrange(x) %>%
    filter(cumsum(x) > 3) %>%
    collect()

  expect_equal(out$x, c(3L, 3L, 4L))
})

# sql_build ---------------------------------------------------------------

test_that("filter generates simple expressions", {
  out <- lazy_frame(x = 1) %>%
    filter(x > 1L) %>%
    sql_build()

  expect_equal(out$where, sql('`x` > 1'))
})
