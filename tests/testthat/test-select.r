context("Select")

df <- as.data.frame(as.list(setNames(1:26, letters)))
tbls <- test_load(df)

test_that("two selects equivalent to one", {
  compare_tbls(tbls, . %>% select(l:s) %>% select(n:o), ref = select(df, n:o))
})

# Database ---------------------------------------------------------------------

test_that("select renames variables (#317)", {
  skip_if_no_sqlite()

  first <- tbls$sqlite %>% select(A = a)
  expect_equal(tbl_vars(first), "A")
  expect_equal(tbl_vars(first %>% select(A)), "A")
  expect_equal(tbl_vars(first %>% select(B = A)), "B")
})

test_that("select preserves grouping vars", {
  skip_if_no_sqlite()

  first <- tbls$sqlite %>% group_by(b) %>% select(a)
  expect_equal(tbl_vars(first), c("b", "a"))
})

test_that("rename handles grouped data (#640)", {
  res <- data_frame(a = 1, b = 2) %>% group_by(a) %>% rename(c = b)
  expect_equal(names(res), c("a", "c"))
})

test_that("rename does not crash with invalid grouped data frame (#640)", {
  df <- data_frame(a = 1:3, b = 2:4, d = 3:5) %>% group_by(a, b)
  df$a <- NULL
  expect_equal(
    df %>% rename(e = d) %>% ungroup,
    data_frame(b = 2:4, e = 3:5)
  )
  expect_equal(
    df %>% rename(e = b) %>% ungroup,
    data_frame(e = 2:4, d = 3:5)
  )
})
