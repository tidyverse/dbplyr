test_that("generates expected SQL for common situations", {
  db <- lazy_frame(g = 1, x = 2)

  expect_snapshot(db %>% count(g))
  expect_snapshot(db %>% count(g, wt = x))
  expect_snapshot(db %>% count(g, sort = TRUE))

  expect_snapshot(db %>% add_count(g, sort = TRUE))
  expect_snapshot(db %>% group_by(g) %>% add_count())
})

test_that("preserves group of input", {
  db <- lazy_frame(g = 1, x = 2)
  expect_equal(db %>% count(g) %>% group_vars(), character())
  expect_equal(db %>% group_by(g) %>% count() %>% group_vars(), "g")
  expect_equal(db %>% group_by(g, x) %>% count() %>% group_vars(), c("g", "x"))

  expect_equal(db %>% group_by(g, x) %>% add_count(g) %>% group_vars(), c("g", "x"))
  expect_equal(db %>% add_count(g) %>% group_vars(), character())
})

test_that("complains about bad names", {
  expect_snapshot(error = TRUE, {
    db <- lazy_frame(g = 1, x = 2)
    db %>% count(g, name = "g")
  })
})

test_that(".drop is not supported", {
  expect_snapshot(error = TRUE, {
    lazy_frame(g = 1) %>%
      add_count(.drop = TRUE)
  })
})
