test_that("generates expected SQL for common situations", {
  db <- lazy_frame(g = 1, x = 2)

  expect_snapshot(db |> count(g))
  expect_snapshot(db |> count(g, wt = x))
  expect_snapshot(db |> count(g, sort = TRUE))

  expect_snapshot(db |> add_count(g, sort = TRUE))
  expect_snapshot(db |> group_by(g) |> add_count())
})

test_that("preserves group of input", {
  db <- lazy_frame(g = 1, x = 2)
  expect_equal(db |> count(g) |> group_vars(), character())
  expect_equal(db |> group_by(g) |> count() |> group_vars(), "g")
  expect_equal(db |> group_by(g, x) |> count() |> group_vars(), c("g", "x"))

  expect_equal(
    db |> group_by(g, x) |> add_count(g) |> group_vars(),
    c("g", "x")
  )
  expect_equal(db |> add_count(g) |> group_vars(), character())
})

test_that("count() does not create groups", {
  # https://github.com/tidyverse/dbplyr/issues/940
  db <- dbplyr::memdb_frame(
    x = c(1, 1, 1, 2, 2),
    y = c("a", "a", "b", "c", "c")
  )

  df <- db |>
    count(x, y) |>
    collect()

  expect <- tibble(
    x = c(1, 1, 2),
    y = c("a", "b", "c"),
    n = c(2L, 1L, 2L)
  )

  expect_equal(group_vars(df), character())
  expect_identical(df, expect)
})

test_that("informs if n column already present, unless overridden", {
  df1 <- lazy_frame(n = c(1, 1, 2, 2, 2))
  expect_message(out <- count(df1, n), "already present")
  expect_equal(colnames(out), c("n", "nn"))

  # not a good idea, but supported
  expect_message(out <- count(df1, n, name = "n"), NA)
  expect_equal(colnames(out), "n")

  expect_message(out <- count(df1, n, name = "nn"), NA)
  expect_equal(colnames(out), c("n", "nn"))

  df2 <- lazy_frame(n = c(1, 1, 2, 2, 2), nn = 1:5)
  expect_message(out <- count(df2, n), "already present")
  expect_equal(colnames(out), c("n", "nn"))

  expect_message(out <- count(df2, n, nn), "already present")
  expect_equal(colnames(out), c("n", "nn", "nnn"))
})

test_that(".drop is not supported", {
  expect_snapshot(error = TRUE, {
    lazy_frame(g = 1) |>
      add_count(.drop = TRUE)
  })
})
