context("Group by")

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

tbls <- test_load(df)

test_that("group_by with add = TRUE adds groups", {
  add_groups1 <- function(tbl) group_by(tbl, x, y, add = TRUE)
  add_groups2 <- function(tbl) group_by(group_by(tbl, x, add = TRUE), y, add = TRUE)

  expect_groups(add_groups1(tbls$df), c("x", "y"))
  expect_groups(add_groups2(tbls$df), c("x", "y"))

  skip_if_no_sqlite()
  expect_groups(add_groups1(tbls$sqlite), c("x", "y"))
  expect_groups(add_groups2(tbls$sqlite), c("x", "y"))
})

test_that("collect, collapse and compute preserve grouping", {
  skip_if_no_sqlite()
  g <- memdb_frame(x = 1:3, y = 1:3) %>% group_by(x, y)

  expect_groups(compute(g), c("x", "y"))
  expect_groups(collapse(g), c("x", "y"))
  expect_groups(collect(g), c("x", "y"))
})

test_that("joins preserve grouping", {
  for (tbl in tbls) {
    g <- group_by(tbl, x)

    expect_groups(inner_join(g, g, by = c("x", "y")), "x")
    expect_groups(left_join (g, g, by = c("x", "y")), "x")
    expect_groups(semi_join (g, g, by = c("x", "y")), "x")
    expect_groups(anti_join (g, g, by = c("x", "y")), "x")
  }
})

test_that("group_by can perform mutate (on database)", {
  mf <- memdb_frame(x = c(3:1), y = c(1:3))
  out <- mf %>%
    group_by(z = x + y) %>%
    summarise(n = n()) %>%
    collect()

  expect_equal(out, tibble(z = 4L, n = 3L))
})
