test_that("ungrouped data collected first", {
  withr::local_options(lifecycle_verbosity = "quiet")
  out <- local_memdb_frame(x = 1:2) |> do(head(.))
  expect_equal(out, tibble(x = 1:2))
})

test_that("named argument become list columns", {
  withr::local_options(lifecycle_verbosity = "quiet")
  mf <- local_memdb_frame(g = rep(1:3, 1:3), x = 1:6) |>
    group_by(g)

  out <- mf |> do(nrow = nrow(.), ncol = ncol(.))
  expect_equal(out$nrow, list(1, 2, 3))
  expect_equal(out$ncol, list(2, 2, 2))
})

test_that("unnamed results bound together by row", {
  withr::local_options(lifecycle_verbosity = "quiet")
  mf <- local_memdb_frame(g = c(1, 1, 2, 2), x = c(3, 9, 4, 9))
  first <- mf |> group_by(g) |> do(head(., 1)) |> ungroup() |> collect()
  expect_equal(first, tibble(g = c(1, 2), x = c(3, 4)))
})

test_that("unnamed results must be data frames", {
  withr::local_options(lifecycle_verbosity = "quiet")
  mf <- local_memdb_frame(g = c(1, 1, 2, 2), x = c(3, 9, 4, 9)) |>
    group_by(g)

  expect_snapshot(error = TRUE, mf |> do(nrow(.)))
})

test_that("Results respect select", {
  withr::local_options(lifecycle_verbosity = "quiet")
  mf <- local_memdb_frame(
    g = c(1, 1, 2, 2),
    x = c(3, 9, 4, 9),
    y = 1:4,
    z = 4:1
  ) |>
    group_by(g)

  expect_message(out <- mf |> select(x) |> do(ncol = ncol(.)))
  expect_equal(out$g, c(1, 2))
  expect_equal(out$ncol, list(2L, 2L))
})

test_that("results independent of chunk_size", {
  withr::local_options(lifecycle_verbosity = "quiet")
  mf <- local_memdb_frame(g = rep(1:3, 1:3), x = 1:6) |>
    group_by(g)

  nrows <- function(group, n) {
    unlist(do(group, nrow = nrow(.), .chunk_size = n)$nrow)
  }

  expect_equal(nrows(mf, 1), c(1, 2, 3))
  expect_equal(nrows(mf, 2), c(1, 2, 3))
  expect_equal(nrows(mf, 10), c(1, 2, 3))
})

test_that("do() argument checking works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  mf <- local_memdb_frame(g = rep(1:3, 1:3), x = 1:6) |>
    group_by(g)

  # mix named and unnamed
  expect_snapshot(error = TRUE, mf |> do(nrow = nrow(.), ncol(.)))

  # multiple unnamed
  expect_snapshot(error = TRUE, mf |> do(nrow(.), ncol(.)))

  # old syntax
  expect_snapshot(error = TRUE, mf |> do(.f = nrow))
})

test_that("do() is deprecated", {
  expect_snapshot(. <- local_memdb_frame(x = 1) |> do(head(.)))
})
