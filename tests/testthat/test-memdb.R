test_that("src_memdb() is deprecated", {
  expect_snapshot(. <- src_memdb())
})

test_that("tbl_memdb() is deprecated", {
  expect_snapshot(. <- tbl_memdb(data.frame(x = 1)))
})
