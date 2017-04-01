context("explain")

test_that("basic pipeline is correct", {
  mf <- memdb_frame(x = 1:5) %>% filter(x > 5)
  expect_output_file(explain(mf), "explain-sqlite.txt")
})
