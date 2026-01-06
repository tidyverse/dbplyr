test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  expect_snapshot(union(lf, lf))
  expect_snapshot(setdiff(lf, lf))
  expect_snapshot(intersect(lf, lf))

  expect_snapshot(union(lf, lf, all = TRUE))
  expect_snapshot(setdiff(lf, lf, all = TRUE))
  expect_snapshot(intersect(lf, lf, all = TRUE))
})

test_that("sql_set_op can be customized", {
  local_methods(
    sql_set_op.TestConnection = function(con, op, ...) {
      switch(
        op,
        "UNION" = "UNION DISTINCT",
        "INTERSECT" = "INTERSECT DISTINCT",
        "EXCEPT" = "EXCEPT DISTINCT",
        op
      )
    }
  )

  lf <- lazy_frame(x = 1)
  expect_snapshot(union(lf, lf))
  expect_snapshot(intersect(lf, lf))
  expect_snapshot(setdiff(lf, lf))
})
