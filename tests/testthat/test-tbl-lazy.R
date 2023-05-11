test_that("adds src class", {
  tb <- tbl_lazy(mtcars, con = simulate_sqlite())
  expect_s3_class(tb, "tbl_SQLiteConnection")
})

test_that("argument src is deprecated", {
  expect_snapshot(error = TRUE, tbl_lazy(mtcars, src = simulate_sqlite()))
})

test_that("cannot convert tbl_lazy to data.frame", {
  expect_snapshot(
    error = TRUE,
    as.data.frame(tbl_lazy(mtcars, con = simulate_sqlite()))
  )
})

test_that("dim() works for tbl_lazy", {
  expect_equal(dim(tbl_lazy(mtcars)), c(NA, 11))
})

test_that("has print method", {
  expect_snapshot(tbl_lazy(mtcars))
})

test_that("support colwise variants", {
  mf <- memdb_frame(x = 1:5, y = factor(letters[1:5]))
  exp <- mf %>% collect() %>% mutate(y = as.character(y))

  expect_message(
    mf1 <- dplyr::mutate_if(mf, is.factor, as.character),
    "on the first 100 rows"
  )
  compare_tbl(mf1, exp)

  mf2 <- dplyr::mutate_at(mf, "y", as.character)
  compare_tbl(mf2, exp)
})

test_that("base source of tbl_lazy is always 'df'", {
  out <- lazy_frame(x = 1, y = 5) %>% sql_build()
  expect_equal(out, base_query(ident("df")))
})
