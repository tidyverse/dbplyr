test_that("reframe is not supported", {
  expect_snapshot(error = TRUE, {
    lazy_frame(x = 1) |> reframe()
  })
})

test_that("summarise peels off a single layer of grouping", {
  mf1 <- memdb_frame(x = 1, y = 1, z = 2) |> group_by(x, y)
  mf2 <- mf1 |> summarise(n = n())
  expect_equal(group_vars(mf2), "x")

  mf3 <- mf2 |> summarise(n = n())
  expect_equal(group_vars(mf3), character())
})

test_that("summarise performs partial evaluation", {
  mf1 <- memdb_frame(x = 1)

  val <- 1
  mf2 <- mf1 |> summarise(y = x == val) |> collect()

  expect_equal(mf2$y, 1)
})

test_that("can't refer to freshly created variables", {
  lf1 <- lazy_frame(a = 1, b = 1)
  expect_snapshot({
    (expect_error(summarise(lf1, a_sum = sum(a), issue_col = sum(a_sum))))

    # works for variables created in `across()`
    # mentions `a_sum`
    (expect_error(summarise(
      lf1,
      across(c(a, b), list(sum = sum)),
      issue_col = sum(a_sum)
    )))
    # mentions `b_sum`
    (expect_error(summarise(
      lf1,
      across(c(a, b), list(sum = sum)),
      issue_col = sum(b_sum)
    )))

    # mentions `across()`
    (expect_error(summarise(
      lf1,
      a_sum = sum(a),
      issue_col = across(a_sum, sum)
    )))
  })
})

test_that("summarise(.groups=)", {
  df <- lazy_frame(x = 1, y = 2) |> group_by(x, y)

  # the `dplyr::` prefix is needed for `check()`
  # should produce a message when called directly by user
  expect_message(eval_bare(
    expr(
      lazy_frame(x = 1, y = 2) |>
        dplyr::group_by(x, y) |>
        dplyr::summarise() |>
        remote_query()
    ),
    env(global_env())
  ))
  expect_snapshot(eval_bare(
    expr(
      lazy_frame(x = 1, y = 2) |>
        dplyr::group_by(x, y) |>
        dplyr::summarise() |>
        remote_query()
    ),
    env(global_env())
  ))

  # should be silent when called in another package
  expect_silent(eval_bare(
    expr(
      lazy_frame(x = 1, y = 2) |>
        dplyr::group_by(x, y) |>
        dplyr::summarise() |>
        remote_query()
    ),
    asNamespace("testthat")
  ))

  expect_equal(df |> summarise() |> group_vars(), "x")
  expect_equal(df |> summarise(.groups = "drop_last") |> group_vars(), "x")
  expect_equal(df |> summarise(.groups = "drop") |> group_vars(), character())
  expect_equal(df |> summarise(.groups = "keep") |> group_vars(), c("x", "y"))

  expect_snapshot(error = TRUE, df |> summarise(.groups = "rowwise"))
})

test_that("summarise can modify grouping variables", {
  lf <- lazy_frame(g = 1, x = 1)

  expect_snapshot((result1 <- lf |> group_by(g) |> summarise(g = g + 1)))
  expect_equal(op_vars(result1), "g")
  expect_snapshot(
    (result2 <- lf |> group_by(g) |> summarise(x = x + 1, g = g + 1))
  )
  expect_equal(op_vars(result2), c("g", "x"))
})

test_that("across() does not select grouping variables", {
  df <- lazy_frame(g = 1, x = 1)

  # SELECT `g`, 0.0 AS `x`
  expect_snapshot(df |> group_by(g) |> summarise(across(.fns = ~0)))
})

test_that("summarise() after select() works #985", {
  df <- memdb_frame(g = 1, x = 1:3)
  expect_equal(
    df |>
      select(x) |>
      summarise(x = mean(x, na.rm = TRUE)) |>
      collect(),
    tibble(x = 2)
  )
})

# .by ----------------------------------------------------------------------

test_that("can group transiently using `.by`", {
  df <- memdb_frame(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))

  out <- summarise(df, x = mean(x, na.rm = TRUE), .by = g) |>
    arrange(g) |>
    collect()

  expect_identical(out$g, c(1, 2))
  expect_identical(out$x, c(3, 2))
  expect_equal(group_vars(out), character())
})

test_that("across doesn't select columns from `.by` #1493", {
  lf <- lazy_frame(g = 1, x = 1)

  out <- lf |>
    summarise(
      across(everything(), ~ sum(..x, na.rm = TRUE)),
      .by = g
    )

  expect_snapshot(out)
  expect_equal(sql_build(out)$select[1], sql("\"g\""))
})

test_that("can't use `.by` with `.groups`", {
  df <- lazy_frame(x = 1)

  expect_snapshot(error = TRUE, {
    summarise(df, .by = x, .groups = "drop")
  })
})

test_that("catches `.by` with grouped-df", {
  df <- lazy_frame(x = 1)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    summarise(gdf, .by = x)
  })
})

# sql-render --------------------------------------------------------------

test_that("quoting for rendering summarized grouped table", {
  out <- copy_to_test("sqlite", tibble(x = 1), name = "verb-summarise") |>
    group_by(x) |>
    summarise(n = n())
  expect_snapshot(out |> sql_render())
  expect_equal(out |> collect(), tibble(x = 1, n = 1L))
})

# sql-build ---------------------------------------------------------------

test_that("summarise generates group_by and select", {
  out <- lazy_frame(g = 1) |>
    group_by(g) |>
    summarise(n = n()) |>
    sql_build()

  expect_equal(out$group_by, sql('"g"'))
  expect_equal(out$select, sql('"g"', 'COUNT(*) AS "n"'))
})


# ops ---------------------------------------------------------------------

test_that("summarise replaces existing", {
  out <- tibble(x = 1, y = 2) |> tbl_lazy() |> summarise(z = 1)
  expect_equal(op_vars(out), "z")
})

test_that("summarised vars are always named", {
  mf <- dbplyr::memdb_frame(a = 1)

  out1 <- mf |> summarise(1) |> op_vars()
  expect_equal(out1, "1")
})

test_that("grouped summary keeps groups", {
  out <- tibble(g = 1, x = 1) |>
    tbl_lazy() |>
    group_by(g) |>
    summarise(y = 1)
  expect_equal(op_vars(out), c("g", "y"))
})

test_that("summarise drops one grouping level", {
  df <- tibble(g1 = 1, g2 = 2, x = 3) |> tbl_lazy() |> group_by(g1, g2)
  out1 <- df |> summarise(y = 1)
  out2 <- out1 |> summarise(y = 2)

  expect_equal(op_grps(out1), "g1")
  expect_equal(op_grps(out2), character())
})


# lazy_select_query -------------------------------------------------------

test_that("can handle rename", {
  lf <- lazy_frame(x = 1:3, y = 3:1)

  out <- lf |>
    group_by(x) |>
    rename(ax = x, by = y) |>
    summarise(mean_by = mean(by, na.rm = TRUE))

  expect_equal(
    out$lazy_query,
    lazy_select_query(
      x = out$lazy_query$x,
      select = list(ax = sym("ax"), mean_by = quo(mean(by, na.rm = TRUE))),
      group_by = syms("ax"),
      select_operation = "summarise",
      group_vars = character()
    ),
    ignore_formula_env = TRUE
  )
})
