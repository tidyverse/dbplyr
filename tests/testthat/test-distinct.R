context("distinct")

df <- tibble(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 2, 1, 2)
)
tbls <- test_load(df)

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  compare_tbls(tbls, . %>% distinct(), ref = unique(df))
})

test_that("distinct for single column works as expected (#1937)", {
  compare_tbls(tbls, . %>% distinct(x, .keep_all = FALSE), ref = df[1, "x"])
  compare_tbls(tbls, . %>% distinct(y, .keep_all = FALSE), ref = df[c(1, 3), "y"])
})

test_that("distinct throws error if column is specified and .keep_all is TRUE", {
  skip_if_no_sqlite()

  expect_error(
    collect(distinct(tbls$sqlite, x, .keep_all = TRUE)),
    "specified columns.*[.]keep_all"
  )
})
