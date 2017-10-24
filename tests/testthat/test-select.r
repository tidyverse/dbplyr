context("select")

df <- as.data.frame(as.list(setNames(1:26, letters)))
tbls <- test_load(df)

test_that("two selects equivalent to one", {
  mf <- memdb_frame(a = 1, b = 1, c = 1, d = 2)

  out <- mf %>%
    select(a:c) %>%
    select(b:c) %>%
    collect()

  expect_named(out, c("b", "c"))
})

test_that("select operates on mutated vars", {
  mf <- memdb_frame(x = c(1, 2, 3), y = c(3, 2, 1))

  df1 <- mf %>%
    mutate(x, z = x + y) %>%
    select(z) %>%
    collect()

  df2 <- mf %>%
    collect() %>%
    mutate(x, z = x + y) %>%
    select(z)

  expect_equal_tbl(df1, df2)
})

test_that("select renames variables (#317)", {
  mf <- memdb_frame(x = 1, y = 2)
  expect_equal_tbl(mf %>% select(A = x), tibble(A = 1))
})

test_that("rename renames variables", {
  mf <- memdb_frame(x = 1, y = 2)
  expect_equal_tbl(mf %>% rename(A = x), tibble(A = 1, y = 2))
})


test_that("can rename multiple vars", {
  mf <- memdb_frame(a = 1, b = 2)
  exp <- tibble(c = 1, d = 2)

  expect_equal_tbl(mf %>% rename(c = a, d = b), exp)
  expect_equal_tbl(mf %>% group_by(a) %>% rename(c = a, d = b), exp)
})

test_that("select preserves grouping vars", {
  mf <- memdb_frame(a = 1, b = 2) %>% group_by(b)
  out <- mf %>% select(a) %>% collect()

  expect_named(out, c("b", "a"))
})
