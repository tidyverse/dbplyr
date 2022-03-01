df <- tibble(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 2, 1, 2)
)
dfs <- test_load(df)

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  dfs %>%
    lapply(. %>% distinct()) %>%
    expect_equal_tbls(unique(df))
})

test_that("distinct for single column equivalent to local unique (#1937)", {
  dfs %>%
    lapply(. %>% distinct(x, .keep_all = FALSE)) %>%
    expect_equal_tbls(unique(df["x"]))

  dfs %>%
    lapply(. %>% distinct(y, .keep_all = FALSE)) %>%
    expect_equal_tbls(unique(df["y"]))
})

test_that("distinct doesn't duplicate colum names if grouped (#354)", {
  df <- lazy_frame(a = 1)
  expect_equal(df %>% group_by(a) %>% distinct() %>% op_vars(), "a")
})

test_that("distinct respects groups", {
  df <- memdb_frame(a = 1:2, b = 1) %>% group_by(a)
  expect_equal(df %>% group_by(a) %>% distinct() %>% op_vars(), c("a", "b"))
})

test_that("distinct returns all columns when .keep_all is TRUE", {
  mf <- memdb_frame(x = c(1, 1, 2, 2), y = 1:4)

  result <- mf %>% distinct(x, .keep_all = TRUE) %>% collect()
  expect_named(result, c("x", "y"))
  expect_equal(result$x, c(1, 2))
  expect_equal(group_vars(result), character())
})

test_that("distinct respects groups when .keep_all is TRUE", {
  mf <- memdb_frame(x = c(1, 1, 2, 2), y = 1:4)

  result <- mf %>% group_by(x) %>% distinct(.keep_all = TRUE) %>% collect()
  expect_named(result, c("x", "y"))
  expect_equal(result$x, c(1, 2))
  expect_equal(group_vars(result), "x")
})

# sql-render --------------------------------------------------------------

test_that("distinct adds DISTINCT suffix", {
  out <- memdb_frame(x = c(1, 1)) %>% distinct()

  expect_match(out %>% sql_render(), "SELECT DISTINCT")
  expect_equal(out %>% collect(), tibble(x = 1))
})

test_that("distinct can compute variables", {
  out <- memdb_frame(x = c(2, 1), y = c(1, 2)) %>% distinct(z = x + y)
  expect_equal(out %>% collect(), tibble(z = 3))
})

test_that("distinct can compute variables when .keep_all is TRUE", {
  out <- memdb_frame(x = c(2, 1), y = c(1, 2)) %>%
    distinct(z = x + y, .keep_all = TRUE) %>%
    collect()

  expect_named(out, c("x", "y", "z"))
  expect_equal(out$z, 3)
})

test_that("distinct respects window_order when .keep_all is TRUE", {
  mf <- memdb_frame(x = c(1, 1, 2, 2), y = 1:4)
  out <- mf %>%
    window_order(desc(y)) %>%
    distinct(x, .keep_all = TRUE)

  expect_equal(out %>% collect(), tibble(x = 1:2, y = c(2, 4)))

  lf <- lazy_frame(x = c(1, 1, 2, 2), y = 1:4)
  expect_snapshot(
    lf %>%
      window_order(desc(y)) %>%
      distinct(x, .keep_all = TRUE)
  )
})

# sql_build ---------------------------------------------------------------

test_that("distinct sets flagged", {
  out1 <- lazy_frame(x = 1) %>%
    select() %>%
    sql_build()
  expect_false(out1$distinct)

  out2 <- lazy_frame(x = 1) %>%
    distinct() %>%
    sql_build()
  expect_true(out2$distinct)
})

# ops ---------------------------------------------------------------------

test_that("distinct produces correct vars", {
  out <- lazy_frame(x = 1, y = 2) %>% distinct()
  expect_equal(op_vars(out), c("x", "y"))

  out <- lazy_frame(x = 1, y = 2, z = 3) %>% distinct(x, y)
  expect_equal(op_vars(out), c("x", "y"))

  out <- lazy_frame(x = 1, y = 2, z = 3) %>% distinct(a = x, b = y)
  expect_equal(op_vars(out), c("a", "b"))

  out <- lazy_frame(x = 1, y = 2, z = 3) %>% group_by(x) %>% distinct(y)
  expect_equal(op_vars(out), c("x", "y"))
})

test_that("distinct produces correct vars when .keep_all is TRUE", {
  lf <- lazy_frame(x = 1, y = 2)
  out <- lf %>% distinct(.keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y"))

  out <- lf %>% distinct(x, .keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y"))

  out <- lf %>% distinct(a = x, .keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y", "a"))

  out <- lazy_frame(x = 1, y = 2, z = 3) %>% group_by(x) %>% distinct(y, .keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y", "z"))
})
