context("distinct")

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

test_that("distinct throws error if column is specified and .keep_all is TRUE", {
  mf <- memdb_frame(x = 1:10)
  expect_error(
    mf %>% distinct(x, .keep_all = TRUE) %>% collect(),
    "specified columns.*[.]keep_all"
  )
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

test_that("distinct has complicated rules", {
  out <- lazy_frame(x = 1, y = 2) %>% distinct()
  expect_equal(op_vars(out), c("x", "y"))

  out <- lazy_frame(x = 1, y = 2, z = 3) %>% distinct(x, y)
  expect_equal(op_vars(out), c("x", "y"))

  out <- lazy_frame(x = 1, y = 2, z = 3) %>% distinct(a = x, b = y)
  expect_equal(op_vars(out), c("a", "b"))

  out <- lazy_frame(x = 1, y = 2, z = 3) %>% group_by(x) %>% distinct(y)
  expect_equal(op_vars(out), c("x", "y"))
})
