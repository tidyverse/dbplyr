test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(sql_build(left_join(lf1, lf2)))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(inner_join(lf, lf))
  expect_snapshot(left_join(lf, lf))
  expect_snapshot(right_join(lf, lf))
  expect_snapshot(full_join(lf, lf))
})

test_that("only disambiguates shared variables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(left_join(lf1, lf2))
  expect_snapshot(left_join(lf1, lf2, by = c("y" = "z")))
})

test_that("disambiguate variables that only differ in case", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(X = 1, y = 2)
  expect_snapshot(left_join(lf1, lf2, by = "y"))
})

test_that("sql_on query doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 3)

  expect_snapshot(inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(left_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(right_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(full_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(semi_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(anti_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
})

test_that("sql_multi_join_vars generates expected SQL", {
  con <- simulate_dbi()

  # left_join(lf(x, a), lf(x, b), by = "x")
  expect_equal(
    sql_multi_join_vars(
      con,
      vars = tibble(
        name = c("x", "a", "b"),
        var = list("x", "a", "b"),
        table = list(1L, 1L, 2L)
      ),
      table_vars = list(left = c("x", "a"), right = c("x", "b"))
    ),
    sql("`left`.*", b = "`b`")
  )

  # full_join(lf(x, a), lf(x, b), by = "x")
  expect_equal(
    sql_multi_join_vars(
      con,
      vars = tibble(
        name = c("x", "a.x", "a.y", "b"),
        table = list(c(1, 2), 1, 2, 2),
        var = list(c("x", "x"), "a", "a", "b")
      ),
      table_vars = list(left = c("x", "a"), right = c("x", "a", "b"))
    ),
    sql(
      x = "COALESCE(`left`.`x`, `right`.`x`)",
      a.x = "`left`.`a`",
      a.y = "`right`.`a`",
      b = "`b`"
    )
  )
})
