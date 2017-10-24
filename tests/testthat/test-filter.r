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

test_that("basic filter works across all backends", {
  dfs <- test_frame(x = 1:5, y = 5:1)

  dfs %>%
    map(. %>% filter(x > 3)) %>%
    expect_equal_tbls()
})

test_that("filter calls windowed versions of sql functions", {
  dfs <- test_frame_windowed(
    x = 1:10,
    g = rep(c(1, 2), each = 5)
  )

  dfs %>%
    map(. %>% group_by(g) %>% filter(row_number(x) < 3)) %>%
    expect_equal_tbls(tibble(g = c(1, 1, 2, 2), x = c(1L, 2L, 6L, 7L)))
})

test_that("recycled aggregates generate window function", {
  dfs <- test_frame_windowed(
    x = 1:10,
    g = rep(c(1, 2), each = 5)
  )

  dfs %>%
    map(. %>% group_by(g) %>% filter(x > mean(x, na.rm = TRUE))) %>%
    expect_equal_tbls(tibble(g = c(1, 1, 2, 2), x = c(4L, 5L, 9L, 10L)))
})

test_that("cumulative aggregates generate window function", {
  dfs <- test_frame_windowed(
    x = c(1:3, 2:4),
    g = rep(c(1, 2), each = 3)
  )

  dfs %>%
    map(. %>%
      group_by(g) %>%
      arrange(x) %>%
      filter(cumsum(x) > 3)
    ) %>%
    expect_equal_tbls(tibble(g = c(1, 2, 2), x = c(3L, 3L, 4L)))
})


