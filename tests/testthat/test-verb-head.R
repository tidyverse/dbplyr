test_that("head limits rows", {
  db <- head(local_memdb_frame(x = 1:100), 10)

  expect_equal(sql_build(db)$limit, 10)
  expect_equal(nrow(collect(db)), 10)
})

test_that("correctly inlines across all verbs", {
  lf <- lazy_frame(x = 1)

  # single table verbs
  expect_selects(lf |> arrange(x) |> head(1), 1)
  expect_selects(lf |> distinct() |> head(1), 1)
  expect_selects(lf |> filter(x) |> head(1), 1)
  expect_selects(lf |> head(1) |> head(1), 1)
  expect_selects(lf |> mutate(y = x + 1) |> head(1), 1)
  expect_selects(lf |> select(x) |> head(1), 1)
  expect_selects(lf |> summarise(y = mean(x)) |> head(1), 1)

  # two table verbs
  lf2 <- lazy_frame(x = 1)
  expect_selects(lf |> left_join(lf2, by = "x") |> head(1), 2)
  expect_selects(lf |> right_join(lf2, by = "x") |> head(1), 2)
  expect_selects(lf |> semi_join(lf2, by = "x") |> head(1), 3)
  expect_selects(lf |> union(lf2) |> head(1), 3)
})

test_that("two heads are equivalent to one", {
  out <- lazy_frame(x = 1:10) |> head(3) |> head(5)
  expect_equal(out$lazy_query$limit, 3)
})

test_that("non-integer automatically truncated", {
  out <- lazy_frame(x = 1:10) |> head(3.5)
  expect_equal(out$lazy_query$limit, 3)
})

test_that("can get 0 rows", {
  db <- local_memdb_frame(x = 1)
  out <- collect(head(db, 0))
  expect_equal(out, tibble(x = double()))
})

test_that("n must be valid", {
  db <- local_memdb_frame(x = 1)

  expect_error(head(db, "x"), "non-negative integer")
  expect_error(head(db, 1:2), "non-negative integer")
  expect_error(head(db, -1), "non-negative integer")
  expect_error(head(db, Inf), NA)
})

test_that("tail not supported", {
  lf <- lazy_frame(x = 1)
  expect_error(tail(lf), "not supported")
})
