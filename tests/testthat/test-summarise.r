context("Summarise")

df <- data.frame(x = rep(1:4, each = 4), y = rep(1:2, each = 8), z = runif(16))
tbls <- test_load(df)

test_that("summarise peels off a single layer of grouping", {
  for (i in seq_along(tbls)) {
    grouped <- group_by(tbls[[i]], x, y)
    summed <- summarise(grouped, n())

    expect_groups(summed, "x", info = names(tbls)[i])
  }
})
