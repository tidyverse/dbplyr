test_that("new_join_by has useful defaults", {
  out <- new_join_by(c("x", "y"))
  expect_equal(out$y, c("x", "y"))
  expect_equal(out$condition, c("==", "=="))
})

test_that("join_by checks inputs", {
  expect_snapshot(error = TRUE, {
    new_join_by("x", c("x", "y"))
    new_join_by("x", "y", c("<", ">"))
  })
})
