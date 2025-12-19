test_that("bind_queries combines multiple lazy tables", {
  lf1 <- lazy_frame(x = 1, y = "a", .name = "lf1")
  lf2 <- lazy_frame(x = 2, y = "b", .name = "lf2")
  lf3 <- lazy_frame(x = 3, y = "c", .name = "lf3")

  expect_snapshot(bind_queries(lf1, lf2, lf3))
})

test_that("bind_queries works with splice operator", {
  lf1 <- lazy_frame(x = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 2, .name = "lf2")
  queries <- list(lf1, lf2)

  expect_snapshot(bind_queries(!!!queries))
})

test_that("bind_queries checks its inputs", {
  lf <- lazy_frame(x = 1)

  expect_snapshot(error = TRUE, {
    bind_queries()
    bind_queries(lf, 1)
  })
})
