context("test-verb-head")


test_that("head limits rows returned", {
  out <- memdb_frame(x = 1:100) %>%
    head(10) %>%
    collect()

  expect_equal(nrow(out), 10)
})

test_that("head limits rows", {
  out <- lazy_frame(x = 1:100) %>%
    head(10) %>%
    sql_build()

  expect_equal(out$limit, 10)
})


test_that("head works with huge whole numbers", {
  out <- memdb_frame(x = 1:100) %>%
    head(1e10) %>%
    collect()

  expect_equal(out, tibble(x = 1:100))
})


# sql_render --------------------------------------------------------------

test_that("head renders to integer fractional input", {
  out <- memdb_frame(x = 1:100) %>%
    head(10.5) %>%
    sql_render()

  expect_match(out, "LIMIT 10$")
})

# ops ---------------------------------------------------------------------

test_that("two heads are equivalent to one", {
  out <- lazy_frame(x = 1:10) %>% head(3) %>% head(5)
  expect_equal(out$ops$args$n, 3)
})
