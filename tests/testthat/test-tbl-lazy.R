context("test-tbl-lazy")

test_that("tbl_lazy adds src class", {
  tb <- tbl_lazy(mtcars, src = simulate_sqlite())
  expect_s3_class(tb, "tbl_SQLiteConnection")
})

test_that("tbl_lazy has print method", {
  expect_known_output(
    tbl_lazy(mtcars),
    test_path("test-tbl-lazy-print.txt"),
    print = TRUE
  )
})

# Single table verbs ------------------------------------------------------

test_that("two arranges equivalent to one", {
  mf <- memdb_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  mf1 <- mf %>% arrange(x, y)
  mf2 <- mf %>% arrange(y) %>% arrange(x)

  expect_equal_tbl(mf1, mf2)
})

test_that("tbl_dbi support colwise variants", {
  mf <- memdb_frame(x = 1:5, y = factor(letters[1:5]))
  exp <- mf %>% collect() %>% mutate(y = as.character(y))

  expect_message(
    mf1 <- mutate_if(mf, is.factor, as.character),
    "on the first 100 rows"
  )
  expect_equal_tbl(mf1, exp)

  mf2 <- mutate_at(mf, "y", as.character)
  expect_equal_tbl(mf2, exp)
})

# Distinct ----------------------------------------------------------------

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
