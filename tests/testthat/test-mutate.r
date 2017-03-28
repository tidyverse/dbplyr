context("mutate")

test_that("two mutates equivalent to one", {
  df <- data.frame(x = c(1, 5, 9), y = c(3, 12, 11))
  tbls <- test_load(df)

  compare_tbls(tbls, . %>% mutate(x2 = x * 2, y4 = y * 4))
})

test_that("mutate happens before summarise", {
  test_f <- function(tbl) {
    res <- tbl %>%
      mutate(x, z = x + y) %>%
      summarise(sum_z = sum(z)) %>%
      collect()
    expect_equal(res$sum_z, 30)
  }

  test_frame(x = c(1, 2, 3), y = c(9, 8, 7)) %>% lapply(test_f)
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



# Window ------------------------------------------------------------------


test_that("mutate calls windowed versions of sql functions", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      mutate(r = as.numeric(row_number(x))) %>%
      collect()
    expect_equal(res$r, c(1, 2, 1, 2))
  }

  df <- data_frame(x = 1:4, g = rep(c(1, 2), each = 2))
  # SQLite and MySQL don't support window functions
  tbls <- test_load(df, ignore = c("sqlite", "mysql"))
  tbls %>% lapply(test_f)
})

test_that("recycled aggregates generate window function", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      mutate(r = x > mean(x)) %>%
      collect()
    expect_equal(res$r, c(FALSE, TRUE, FALSE, TRUE))
  }

  df <- data_frame(x = 1:4, g = rep(c(1, 2), each = 2))
  # SQLite and MySQL don't support window functions
  tbls <- test_load(df, ignore = c("sqlite", "mysql"))
  tbls %>% lapply(test_f)
})

test_that("cumulative aggregates generate window function", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      arrange(x) %>%
      mutate(r = cumsum(x)) %>%
      collect()
    expect_equal(res$r, c(1, 3, 3, 7))
  }

  df <- data_frame(x = 1:4, g = rep(c(1, 2), each = 2))
  # SQLite and MySQL don't support window functions
  tbls <- test_load(df, ignore = c("sqlite", "mysql"))
  tbls %>% lapply(test_f)
})
