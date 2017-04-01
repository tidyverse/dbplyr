context("explain")

df <- as.data.frame(as.list(setNames(1:26, letters)))
tbls <- test_load(df)

test_that("explain works", {
  expect_error(explain(tbls$sqlite), NA)
})
