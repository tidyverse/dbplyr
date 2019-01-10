context("test-explain.R")

test_that("basic pipeline is correct", {
  mf <- memdb_frame(x = 1:5, .name = "XYZ") %>% filter(x > 5)
  expect_output_file(explain(mf), "test-explain-sqlite.txt")
})
