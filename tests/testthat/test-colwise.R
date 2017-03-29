context("colwise")

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

