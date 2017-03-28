context("mutate")

test_that("mutate computed before summarise", {
  mf <- memdb_frame(x = c(1, 2, 3), y = c(9, 8, 7))

  out <- mutate(mf, z = x + y) %>%
    summarise(sum_z = sum(z)) %>%
    collect()

  expect_equal(out$sum_z, 30)
})

test_that("two mutates equivalent to one", {
  mf <- memdb_frame(x = c(1, 5, 9), y = c(3, 12, 11))

  df1 <- mf %>% mutate(x2 = x * 2, y4 = y * 4) %>% collect()
  df2 <- mf %>% collect() %>% mutate(x2 = x * 2, y4 = y * 4)
  expect_equal_tbl(df1, df2)
})

test_that("can refer to fresly created values", {
  out1 <- memdb_frame(x = 1) %>%
    mutate(y = x + 1, z = y + 1) %>%
    collect()
  expect_equal(out1, tibble(x = 1, y = 2, z = 3))

  out2 <- memdb_frame(x = 1) %>%
    mutate(x = x + 1, x = x + 1) %>%
    collect()
  expect_equal(out2, tibble(x = 3))
})

test_that("queries are not nested unnecessarily", {
  # Should only be one query deep
  sql <- memdb_frame(x = 1) %>%
    mutate(y = x + 1, a = y + 1, b = y + 1) %>%
    sql_build()

  expect_s3_class(sql$from, "select_query")
  expect_s3_class(sql$from$from, "ident")
})

# SQL generation -----------------------------------------------------------

test_that("mutate calls windowed versions of sql functions", {
  dfs <- test_frame_windowed(x = 1:4, g = rep(c(1, 2), each = 2))
  out <- map(dfs, . %>% group_by(g) %>% mutate(r = as.numeric(row_number(x))))

  expect_equal(out$df$r, c(1, 2, 1, 2))
  expect_equal_tbls(out)
})

test_that("recycled aggregates generate window function", {
  dfs <- test_frame_windowed(x = 1:4, g = rep(c(1, 2), each = 2))
  out <- map(dfs, . %>% group_by(g) %>% mutate(r = x > mean(x)))

  expect_equal(out$df$r, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal_tbls(out)
})

test_that("cumulative aggregates generate window function", {
  dfs <- test_frame_windowed(x = 1:4, g = rep(c(1, 2), each = 2))
  out <- map(dfs, . %>%
    group_by(g) %>%
    arrange(x) %>%
    mutate(r = as.numeric(cumsum(x)))
  )

  expect_equal(out$df$r, c(1, 3, 3, 7))
  expect_equal_tbls(out)
})
