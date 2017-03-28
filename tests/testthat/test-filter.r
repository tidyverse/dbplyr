context("filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

tbls <- test_load(df)

test_that("filter results independent of data tbl (simple)", {
  skip_if_no_sqlite()

  expected <- df[df$a > 6, , drop = FALSE]
  compare_tbls(tbls[c("df", "sqlite")], function(x) {
    filter(x, a > 6)
  }, expected)
})

test_that("filter captures local variables", {
  sel <- c("d", "g", "a")
  expected <- df[df$b %in% sel, , drop = FALSE]

  compare_tbls(tbls, function(x) x %>% filter(b %in% sel), ref = expected)
})

test_that("two filters equivalent to one", {
  expected <- filter(df, a > 4 & b == "a")

  compare_tbls(tbls, function(x) x %>% filter(a > 4) %>% filter(b == "a"),
    ref = expected)
})


# Window functions --------------------------------------------------------


test_that("filter calls windowed versions of sql functions", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      filter(row_number(x) < 3) %>%
      collect()
    expect_equal(res$x, c(1, 2, 6, 7))
    expect_equal(res$g, c(1, 1, 2, 2))
  }

  df <- data_frame(x = 1:10, g = rep(c(1, 2), each = 5))
  # SQLite and MySQL don't support window functions
  tbls <- test_load(df, ignore = c("sqlite", "mysql"))
  tbls %>% lapply(test_f)
})

test_that("recycled aggregates generate window function", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      filter(x > mean(x)) %>%
      collect()
    expect_equal(res$x, c(4, 5, 9, 10))
    expect_equal(res$g, c(1, 1, 2, 2))
  }

  df <- data_frame(x = 1:10, g = rep(c(1, 2), each = 5))
  # SQLite and MySQL don't support window functions
  tbls <- test_load(df, ignore = c("sqlite", "mysql"))
  tbls %>% lapply(test_f)
})

test_that("cumulative aggregates generate window function", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      arrange(x) %>%
      filter(cumsum(x) > 3) %>%
      collect()
    expect_equal(res$x, c(3, 3, 4))
    expect_equal(res$g, c(1, 2, 2))
  }

  df <- data_frame(x = c(1:3, 2:4), g = rep(c(1, 2), each = 3))
  # SQLite and MySQL don't support window functions
  tbls <- test_load(df, ignore = c("sqlite", "mysql"))
  tbls %>% lapply(test_f)
})


