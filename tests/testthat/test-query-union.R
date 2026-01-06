test_that("set ops captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- union(lf1, lf2) |> sql_build()
  expect_s3_class(out, "union_query")
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)
  expect_snapshot(union(lf, lf))
  expect_snapshot(union_all(lf, lf))
})

test_that("sql_set_op_method can be customized", {
  local_methods(
    sql_set_op_method.TestConnection = function(con, op, ...) {
      switch(op, "UNION" = "UNION DISTINCT", op)
    }
  )

  lf <- lazy_frame(x = 1)
  expect_match(remote_query(union(lf, lf)), "UNION DISTINCT")
})
