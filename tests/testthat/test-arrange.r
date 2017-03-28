context("Arrange")

test_that("two arranges equivalent to one", {
  df1 <- tribble(
    ~x,  ~y,
    2,  1,
    2,  -1,
    1,  1
  )
  tbls <- test_load(df1)

  single <- df1 %>% arrange(x, y)
  compare_tbls(tbls, function(x) x %>% arrange(y) %>% arrange(x), ref = single)
})
