context("test-escape.R")

# Test full logical escaping round trip on databases
test_that("logical values correctly escaped", {
  src <- test_frame(x = c(TRUE, FALSE))
  dfs <- src %>% lapply(. %>% mutate(y = x == TRUE))
  out <- dfs %>% lapply(collect)

  # SQLite treats as integers
  expect_identical(out$sqlite$y, c(1L, 0L))

  # MySQL converts to doubles
  if (!is.null(out$mysql))
    expect_identical(out$mysql$y, c(1, 0))

  # PostgresSQL keeps as logical
  if (!is.null(out$postgres))
    expect_identical(out$postgres$y, c(TRUE, FALSE))
})
