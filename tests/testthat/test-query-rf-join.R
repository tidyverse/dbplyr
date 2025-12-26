test_that("sql_rf_join_vars generates expected SQL", {
  con <- simulate_dbi()

  # full_join(lf(x, a), lf(x, b), by = "x")
  expect_equal(
    sql_rf_join_vars(
      con,
      type = "full",
      vars = list(
        name = c("x", "a.x", "a.y", "b"),
        x = c("x", "a", NA, NA),
        y = c("x", NA, "a", "b"),
        all_x = c("x", "a"),
        all_y = c("x", "a", "b")
      ),
      x_as = table_path("left"),
      y_as = table_path("right"),
      use_star = TRUE,
      qualify_all_columns = FALSE
    ),
    sql(
      'COALESCE("left"."x", "right"."x") AS "x"',
      '"left"."a" AS "a.x"',
      '"right"."a" AS "a.y"',
      '"b"'
    )
  )

  # disambiguate variables that only differ in case
  expect_equal(
    sql_rf_join_vars(
      con,
      type = "full",
      vars = list(
        name = c("a", "b.x", "b.y"),
        x = c("a", "b", NA),
        y = c("a", NA, "B"),
        all_x = c("a", "b"),
        all_y = c("a", "B")
      ),
      x_as = table_path("left"),
      y_as = table_path("right"),
      use_star = TRUE,
      qualify_all_columns = FALSE
    ),
    sql(
      'COALESCE("left"."a", "right"."a") AS "a"',
      '"left"."b" AS "b.x"',
      '"right"."B" AS "b.y"'
    )
  )
})
