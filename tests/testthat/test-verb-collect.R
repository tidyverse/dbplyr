test_that("collect equivalent to as.data.frame/as_tibble", {
  mf <- local_memdb_frame(letters = letters)

  expect_equal(as.data.frame(mf), data.frame(letters, stringsAsFactors = FALSE))
  expect_equal(as_tibble(mf), tibble(letters))
  expect_equal(collect(mf), tibble(letters))
})

test_that("explicit collection returns all data", {
  n <- 1e5 + 10 # previous default was 1e5
  big <- local_memdb_frame(x = seq_len(n))

  nrow1 <- big |> as.data.frame() |> nrow()
  nrow2 <- big |> as_tibble() |> nrow()
  nrow3 <- big |> collect() |> nrow()

  expect_equal(nrow1, n)
  expect_equal(nrow2, n)
  expect_equal(nrow3, n)
})

test_that("collect() handles DBI error", {
  mf <- local_memdb_frame(x = 1)
  expect_snapshot(
    (expect_error(mf |> mutate(a = sql("invalid sql")) |> collect())),
    transform = snap_transform_dbi
  )
})
