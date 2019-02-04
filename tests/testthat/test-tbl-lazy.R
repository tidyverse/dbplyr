context("test-tbl-lazy.R")

test_that("adds src class", {
  tb <- tbl_lazy(mtcars, con = simulate_sqlite())
  expect_s3_class(tb, "tbl_SQLiteConnection")
})

test_that("has print method", {
  expect_known_output(
    tbl_lazy(mtcars),
    test_path("test-tbl-lazy-print.txt"),
    print = TRUE
  )
})

test_that("support colwise variants", {
  mf <- memdb_frame(x = 1:5, y = factor(letters[1:5]))
  exp <- mf %>% collect() %>% mutate(y = as.character(y))

  expect_message(
    mf1 <- mutate_if(mf, is.factor, as.character),
    "on the first 100 rows"
  )
  expect_equal_tbl(mf1, exp)

  mf2 <- mutate_at(mf, "y", as.character)
  expect_equal_tbl(mf2, exp)
})

test_that("base source of tbl_lazy is always 'df'", {
  out <- lazy_frame(x = 1, y = 5) %>% sql_build()
  expect_equal(out, ident("df"))
})
