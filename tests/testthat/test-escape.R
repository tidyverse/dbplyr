context("escape")

# See test-sql-escape for SQL generation tests. These tests that the
# generate SQL actually works across the three main open source backends

test_that("multiplication works", {
  dfs <- test_frame(x = c(1, NA)) %>%
    map(. %>% mutate(y = coalesce(x > 0, TRUE)))
  out <- dfs %>% map(collect)

  # SQLite treats as integers
  expect_identical(out$sqlite$y, c(1L, 1L))

  # MySQL converts to doubles
  if (!is.null(out$mysql))
    expect_identical(out$mysql$y, c(1, 1))

  # PostgresSQL keeps as logical
  if (!is.null(out$postgres))
    expect_identical(out$postgres$y, c(TRUE, TRUE))
})
