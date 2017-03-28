context("do")

df <- data.frame(
  g = c(1, 2, 2, 3, 3, 3),
  x = 1:6,
  y = 6:1
)

tbls <- test_load(df)
grp <- lapply(tbls, function(x) x %>% group_by(g))

test_that("ungrouped data collected first", {
  out <- memdb_frame(x = 1:2) %>% do(head(.))
  expect_equal(out, tibble(x = 1:2))
})

test_that("named argument become list columns", {
  skip_if_no_sqlite()

  out <- grp$sqlite %>% do(nrow = nrow(.), ncol = ncol(.))
  expect_equal(out$nrow, list(1, 2, 3))
  expect_equal(out$ncol, list(3, 3, 3))
})

test_that("unnamed results bound together by row", {
  skip_if_no_sqlite()

  first <- grp$sqlite %>% do(head(., 1))

  expect_equal(nrow(first), 3)
  expect_equal(first$g, 1:3)
  expect_equal(first$x, c(1, 2, 4))
})

test_that("Results respect select", {
  skip_if_no_sqlite()

  smaller <- grp$sqlite %>% select(g, x) %>% do(ncol = ncol(.))
  expect_equal(smaller$ncol, list(2, 2, 2))
})

test_that("grouping column not repeated", {
  skip_if_no_sqlite()

  out <- grp$sqlite %>% do(names = names(.))
  expect_equal(out$names[[1]], c("g", "x", "y"))
})

test_that("results independent of chunk_size", {
  skip_if_no_sqlite()
  nrows <- function(group, n) {
    unlist(do(group, nrow = nrow(.), .chunk_size = n)$nrow)
  }

  expect_equal(nrows(grp$sqlite, 1), c(1, 2, 3))
  expect_equal(nrows(grp$sqlite, 2), c(1, 2, 3))
  expect_equal(nrows(grp$sqlite, 10), c(1, 2, 3))
})

test_that("handling of empty data frames in do", {
  blankdf <- function(x) data.frame(blank = numeric(0))
  dat <- data.frame(a = 1:2, b = factor(1:2))
  res <- dat %>% group_by(b) %>% do(blankdf(.))
  expect_equal(names(res), c("b", "blank"))
})

