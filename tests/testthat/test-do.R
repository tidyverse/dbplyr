context("do")

test_that("ungrouped data collected first", {
  out <- memdb_frame(x = 1:2) %>% do(head(.))
  expect_equal(out, tibble(x = 1:2))
})

test_that("named argument become list columns", {
  mf <- memdb_frame(
    g = rep(1:3, 1:3),
    x = 1:6
  ) %>% group_by(g)

  out <- mf %>% do(nrow = nrow(.), ncol = ncol(.))
  expect_equal(out$nrow, list(1, 2, 3))
  expect_equal(out$ncol, list(2, 2, 2))
})

test_that("unnamed results bound together by row", {
  mf <- memdb_frame(
    g = c(1, 1, 2, 2),
    x = c(3, 9, 4, 9)
  ) %>% group_by(g)

  first <- mf %>% do(head(., 1))
  expect_equal_tbl(first, tibble(g = c(1, 2), x = c(3, 4)))
})

test_that("Results respect select", {
  mf <- memdb_frame(
    g = c(1, 1, 2, 2),
    x = c(3, 9, 4, 9),
    y = 1:4,
    z = 4:1
  ) %>% group_by(g)

  out <- mf %>% select(x) %>% do(ncol = ncol(.))
  expect_equal(out$g, c(1, 2))
  expect_equal(out$ncol, list(2L, 2L))
})

test_that("results independent of chunk_size", {
  mf <- memdb_frame(
    g = rep(1:3, 1:3),
    x = 1:6
  ) %>% group_by(g)

  nrows <- function(group, n) {
    unlist(do(group, nrow = nrow(.), .chunk_size = n)$nrow)
  }

  expect_equal(nrows(mf, 1), c(1, 2, 3))
  expect_equal(nrows(mf, 2), c(1, 2, 3))
  expect_equal(nrows(mf, 10), c(1, 2, 3))
})

