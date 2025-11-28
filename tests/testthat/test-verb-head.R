test_that("head limits rows", {
  db <- head(memdb_frame(x = 1:100), 10)

  expect_equal(sql_build(db)$limit, 10)
  expect_equal(nrow(collect(db)), 10)
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
  db <- memdb_frame(x = 1)
  out <- collect(head(db, 0))
  expect_equal(out, tibble(x = double()))
})

test_that("n must be valid", {
  db <- memdb_frame(x = 1)

  expect_error(head(db, "x"), "non-negative integer")
  expect_error(head(db, 1:2), "non-negative integer")
  expect_error(head(db, -1), "non-negative integer")
  expect_error(head(db, Inf), NA)
})

test_that("tail not supported", {
  lf <- lazy_frame(x = 1)
  expect_error(tail(lf), "not supported")
})
