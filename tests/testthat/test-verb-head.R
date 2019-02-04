context("test-verb-head")

# ops ---------------------------------------------------------------------

test_that("two heads are equivalent to one", {
  out <- lazy_frame(x = 1:10) %>% head(3) %>% head(5)
  expect_equal(out$ops$args$n, 3)
})
