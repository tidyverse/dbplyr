context("Mutate")

test_that("two mutates equivalent to one", {
  df <- data.frame(x = c(1, 5, 9), y = c(3, 12, 11))
  tbls <- test_load(df)

  compare_tbls(tbls, . %>% mutate(x2 = x * 2, y4 = y * 4))
})
