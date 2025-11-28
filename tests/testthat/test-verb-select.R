test_that("select quotes correctly", {
  out <- memdb_frame(x = 1, y = 1) |>
    select(x) |>
    collect()
  expect_equal(out, tibble(x = 1))
})

test_that("select can rename", {
  out <- memdb_frame(x = 1, y = 2) |>
    select(y = x) |>
    collect()
  expect_equal(out, tibble(y = 1))
})

test_that("two selects equivalent to one", {
  mf <- memdb_frame(a = 1, b = 1, c = 1, d = 2)

  out <- mf |>
    select(a:c) |>
    select(b:c) |>
    collect()

  expect_named(out, c("b", "c"))
})

test_that("select after distinct produces subquery", {
  lf <- lazy_frame(x = 1, y = 1:2)
  expect_snapshot(
    lf |> distinct() |> select(x)
  )

  out <- lf |> distinct() |> select(x)
  lq <- out$lazy_query
  expect_false(lq$distinct)
  expect_true(lq$x$distinct)
})

test_that("select after arrange produces subquery, if needed", {
  lf <- lazy_frame(x = 1)

  # shouldn't inline
  out <- lf |> mutate(z = 2) |> arrange(x, z) |> select(x)
  # should inline
  out2 <- lf |> mutate(z = 2) |> arrange(x, z) |> select(x, z)

  inner_query <- out$lazy_query$x
  expect_s3_class(inner_query, "lazy_select_query")
  expect_equal(
    inner_query$order_by,
    list(quo(x), quo(z)),
    ignore_formula_env = TRUE
  )
  expect_equal(
    op_vars(inner_query),
    c("x", "z")
  )
  expect_equal(op_vars(out$lazy_query), "x")

  # order vars in a subquery are dropped
  expect_equal(
    inner_query[setdiff(names(inner_query), "order_vars")],
    out2$lazy_query[setdiff(names(out2$lazy_query), "order_vars")]
  )
})


test_that("rename/relocate after distinct is inlined #1141", {
  lf <- lazy_frame(x = 1, y = 1:2)
  expect_snapshot({
    lf |> distinct() |> rename(z = y)
    lf |> distinct() |> relocate(y)
  })

  out <- lf |> distinct() |> rename(z = y)
  lq <- out$lazy_query
  expect_true(lq$distinct)

  out <- lf |> distinct() |> relocate(y)
  lq <- out$lazy_query
  expect_true(lq$distinct)
})

test_that("select operates on mutated vars", {
  mf <- memdb_frame(x = c(1, 2, 3), y = c(3, 2, 1))

  df1 <- mf |>
    mutate(x, z = x + y) |>
    select(z) |>
    collect()

  df2 <- mf |>
    collect() |>
    mutate(x, z = x + y) |>
    select(z)

  compare_tbl(df1, df2)
})

test_that("select renames variables (#317)", {
  mf <- memdb_frame(x = 1, y = 2)
  compare_tbl(mf |> select(A = x), tibble(A = 1))
})

test_that("rename renames variables", {
  mf <- memdb_frame(x = 1, y = 2)
  compare_tbl(mf |> rename(A = x), tibble(A = 1, y = 2))
})

test_that("can rename multiple vars", {
  mf <- memdb_frame(a = 1, b = 2)
  exp <- tibble(c = 1, d = 2)

  compare_tbl(mf |> rename(c = a, d = b), exp)
  compare_tbl(mf |> group_by(a) |> rename(c = a, d = b), exp |> group_by(c))
})

test_that("can rename with a function", {
  mf <- memdb_frame(a = 1, b = 2)

  expect_named(mf |> rename_with(toupper) |> collect(), c("A", "B"))
  expect_named(mf |> rename_with(toupper, 1) |> collect(), c("A", "b"))
})

test_that("select preserves grouping vars", {
  mf <- memdb_frame(a = 1, b = 2) |> group_by(b)
  expect_snapshot(out <- mf |> select(a) |> collect())

  expect_named(out, c("b", "a"))
})

test_that("select handles order vars", {
  lf <- lazy_frame(x = 1, y = 1, z = 1)
  # can drop order vars
  expect_equal(lf |> window_order(y) |> select(-y) |> op_sort(), list())
  expect_equal(
    lf |> window_order(desc(y)) |> select(-y) |> op_sort(),
    list()
  )
  # can rename order vars
  expect_equal(
    lf |> window_order(y) |> select(y2 = y) |> op_sort(),
    list(expr(y2))
  )
  expect_equal(
    lf |> window_order(desc(y)) |> select(y2 = y) |> op_sort(),
    list(expr(desc(y2)))
  )
  # keeps sort order
  expect_equal(
    lf |> window_order(x, y) |> select(y2 = y, x) |> op_sort(),
    list(expr(x), expr(y2))
  )
})

test_that("select doesn't relocate grouping vars to the front", {
  mf <- memdb_frame(a = 1, b = 2) |> group_by(b)
  expect_equal(mf |> select(a, b) |> op_vars(), c("a", "b"))
})

test_that("relocate works", {
  mf <- memdb_frame(a = 1, b = 2, c = 1) |> group_by(b)

  out1 <- mf |> relocate(c) |> collect()
  expect_named(out1, c("c", "a", "b"))
  out2 <- mf |> relocate(a, .after = c) |> collect()
  expect_named(out2, c("b", "c", "a"))
})

test_that("relocate can rename variables", {
  mf <- memdb_frame(a = 1, b = 2, c = 1) |> group_by(b)

  out1 <- mf |> relocate(d = b) |> collect()
  expect_named(out1, c("d", "a", "c"))
  expect_equal(group_vars(out1), "d")
})

test_that("only add step if necessary", {
  lf <- lazy_frame(x = 1:3, y = 1:3)
  expect_equal(lf |> select(everything()), lf)
  expect_equal(lf |> select(x, y), lf)

  expect_equal(lf |> rename(x = x), lf)
  expect_equal(lf |> rename(), lf)

  expect_equal(lf |> relocate(x, y), lf)
  expect_equal(lf |> relocate(), lf)
})

test_that("select() after left_join() is inlined", {
  lf1 <- lazy_frame(x = 1, a = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "lf2")

  expect_snapshot(
    (out <- left_join(lf1, lf2, by = "x") |>
      select(b, x))
  )
  expect_equal(op_vars(out), c("b", "x"))

  expect_snapshot(
    (out <- left_join(lf1, lf2, by = "x") |>
      relocate(b))
  )
  expect_equal(op_vars(out), c("b", "x", "a"))
  expect_equal(out$lazy_query$vars$var, c("b", "x", "a"))
  expect_equal(out$lazy_query$vars$table, c(2L, 1L, 1L))

  out <- left_join(lf1, lf2, by = "x") |>
    transmute(b, x = x + 1)
  expect_s3_class(out$lazy_query, "lazy_select_query")
})

test_that("select() after semi_join() is inlined", {
  lf1 <- lazy_frame(x = 1, a = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "lf2")

  expect_snapshot(
    (out <- semi_join(lf1, lf2, by = "x") |>
      select(x, a2 = a))
  )
  expect_equal(op_vars(out), c("x", "a2"))

  expect_snapshot(
    (out <- anti_join(lf1, lf2, by = "x") |>
      relocate(a))
  )
  expect_equal(op_vars(out), c("a", "x"))

  out <- semi_join(lf1, lf2, by = "x") |>
    transmute(a, x = x + 1)
  expect_s3_class(out$lazy_query, "lazy_select_query")
})

test_that("select() after join handles previous select", {
  lf <- lazy_frame(x = 1, y = 1, z = 1) |>
    group_by(x, y, z) |>
    select(x, y2 = y, z) |>
    semi_join(
      lazy_frame(x = 1),
      by = "x"
    ) |>
    select(x2 = x, y3 = y2, z)

  expect_equal(op_vars(lf), c("x2", "y3", "z"))
  expect_equal(
    lf$lazy_query$vars,
    tibble(
      name = c("x2", "y3", "z"),
      var = c("x", "y", "z")
    )
  )
  expect_equal(op_grps(lf), c("x2", "y3", "z"))
  expect_snapshot(print(lf))

  lf2 <- lazy_frame(x = 1, y = 1, z = 1) |>
    group_by(x, y, z) |>
    select(x, y2 = y, z) |>
    left_join(
      lazy_frame(x = 1, y = 1),
      by = "x"
    ) |>
    select(x2 = x, y3 = y2, z)

  expect_equal(op_vars(lf2), c("x2", "y3", "z"))
  vars2 <- lf2$lazy_query$vars
  expect_equal(vars2$var, c("x", "y", "z"))
  expect_equal(vars2$table, c(1L, 1L, 1L))

  expect_equal(op_grps(lf2), c("x2", "y3", "z"))
  expect_snapshot(print(lf2))
})

test_that("select() afer join keeps grouping", {
  lf1 <- lazy_frame(x = 1, y = 1) |> group_by(y)
  lf2 <- lazy_frame(x = 1, z = 1) |> group_by(z)

  # just to be sure check without select/renaming
  expect_equal(left_join(lf1, lf2, by = "x") |> op_grps(), "y")

  # rename grouping variable
  expect_equal(
    left_join(lf1, lf2, by = "x") |>
      select(y2 = y) |>
      op_grps(),
    "y2"
  )
})

test_that("select() produces nice error messages", {
  lf <- lazy_frame(x = 1)

  expect_snapshot(error = TRUE, {
    lf |> select(non_existent)
    lf |> select(non_existent + 1)
  })

  expect_snapshot(error = TRUE, {
    lf |> relocate(non_existent)
    lf |> relocate(non_existent + 1)
  })

  expect_snapshot(error = TRUE, {
    # no name
    lf |> rename(x)
    # non-existing column
    lf |> rename(y = non_existent)
    lf |> rename(y = non_existent + 1)
  })

  expect_snapshot(error = TRUE, {
    lf |> rename_with(toupper, .cols = non_existent)
    lf |> rename_with(toupper, .cols = non_existent + 1)
  })
})

test_that("where() isn't suppored", {
  lf <- lazy_frame(x = 1)
  expect_snapshot(error = TRUE, {
    lf |> select(where(is.integer))
  })
})

test_that("arranged computed columns are not inlined away", {
  lf <- lazy_frame(x = 1)

  # shouldn't inline
  out <- lf |> mutate(z = 2) |> arrange(x, z) |> select(x)
  # should inline
  out2 <- lf |> mutate(z = 2) |> arrange(x, z) |> select(x, z)

  inner_query <- out$lazy_query$x
  expect_snapshot({
    lf |> mutate(z = 1) |> arrange(x, z) |> select(x)
  })
  expect_s3_class(inner_query, "lazy_select_query")
  expect_equal(
    inner_query$order_by,
    list(quo(x), quo(z)),
    ignore_formula_env = TRUE
  )
  expect_equal(op_vars(inner_query), c("x", "z"))
  expect_equal(op_vars(out$lazy_query), "x")
  expect_equal(
    # order vars in a subquery are dropped
    inner_query[setdiff(names(inner_query), "order_vars")],
    out2$lazy_query[setdiff(names(out2$lazy_query), "order_vars")]
  )
})

# sql_render --------------------------------------------------------------

test_that("multiple selects are collapsed", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(lf |> select(2:1) |> select(2:1))
  expect_snapshot(lf |> select(2:1) |> select(2:1) |> select(2:1))
  expect_snapshot(lf |> select(x1 = x) |> select(x2 = x1))
})

test_that("mutate collapses over nested select", {
  lf <- lazy_frame(g = 0, x = 1, y = 2)

  expect_snapshot(lf |> mutate(a = 1, b = 2) |> select(a))
  expect_snapshot(lf |> mutate(a = 1, b = 2) |> select(x))
})

test_that("output is styled", {
  local_reproducible_output(crayon = TRUE)
  withr::local_options(dbplyr_highlight = cli::combine_ansi_styles("blue"))

  lf <- lazy_frame(x = 1, y = 1, z = 1)
  out <- lf |>
    group_by(x) |>
    mutate(y = mean(y, na.rm = TRUE), z = z + 1) |>
    filter(z == 1) |>
    left_join(lf, by = "x")

  expect_snapshot(show_query(out, sql_options = sql_options(cte = TRUE)))
})

# sql_build -------------------------------------------------------------

test_that("select picks variables", {
  out <- lazy_frame(x1 = 1, x2 = 1, x3 = 2) |>
    select(x1:x2) |>
    sql_build()

  expect_equal(out$select, sql("x1" = "`x1`", "x2" = "`x2`"))
})

test_that("select renames variables", {
  out <- lazy_frame(x1 = 1, x2 = 1, x3 = 2) |>
    select(y = x1, z = x2) |>
    sql_build()

  expect_equal(out$select, sql("y" = "`x1`", "z" = "`x2`"))
})

test_that("select can refer to variables in local env", {
  vars <- c("x", "y")
  out <- lazy_frame(x = 1, y = 1, z = 1) |>
    select(dplyr::one_of(vars)) |>
    sql_build()

  expect_equal(out$select, sql("x" = "`x`", "y" = "`y`"))
})

test_that("rename preserves existing vars", {
  out <- lazy_frame(x = 1, y = 1) |>
    rename(z = y) |>
    sql_build()

  expect_equal(out$select, sql("x" = "`x`", "z" = "`y`"))
})


# ops ---------------------------------------------------------------------

test_that("select reduces variables", {
  out <- mtcars |> tbl_lazy() |> select(mpg:disp)
  expect_equal(op_vars(out), c("mpg", "cyl", "disp"))
})

test_that("rename preserves existing", {
  out <- tibble(x = 1, y = 2) |> tbl_lazy() |> rename(z = y)
  expect_equal(op_vars(out), c("x", "z"))
})

test_that("rename renames grouping vars", {
  df <- lazy_frame(a = 1, b = 2)
  expect_equal(df |> group_by(a) |> rename(c = a) |> op_grps(), "c")
})

test_that("mutate preserves grouping vars (#396)", {
  df <- lazy_frame(a = 1, b = 2, c = 3) |> group_by(a, b)
  expect_equal(df |> mutate(a = 1) |> op_grps(), c("a", "b"))
  expect_equal(df |> mutate(b = 1) |> op_grps(), c("a", "b"))
})

test_that("select after arrange(desc()) works (#1206)", {
  out <- lazy_frame(x = 1, y = 1) |>
    arrange(desc(x)) |>
    select(x)
  expect_equal(op_vars(out), "x")
})


# lazy_select_query -------------------------------------------------------

test_that("select, relocate, and rename work", {
  lf <- lazy_frame(x = 1, y = 1)

  expect_equal(
    lf |>
      select(x) |>
      _$lazy_query |>
      _$select,
    new_lazy_select(exprs(x = x))
  )

  expect_equal(
    lf |>
      relocate(y) |>
      _$lazy_query |>
      _$select,
    new_lazy_select(exprs(y = y, x = x))
  )

  expect_equal(
    lf |>
      rename(b = y, a = x) |>
      _$lazy_query |>
      _$select,
    new_lazy_select(exprs(a = x, b = y))
  )
})

test_that("renaming handles groups correctly", {
  lf <- lazy_frame(x = 1, y = 1) |>
    group_by(x) |>
    rename(ax = x)

  result <- lf$lazy_query
  expect_equal(
    result$select,
    new_lazy_select(exprs(ax = x, y = y))
  )

  expect_equal(result$group_vars, "ax")
  expect_equal(op_grps(result), "ax")

  result <- lf |>
    rename(x = ax) |>
    _$lazy_query

  expect_equal(
    result$select,
    new_lazy_select(exprs(x = x, y = y))
  )

  expect_equal(result$group_vars, "x")
  expect_equal(op_grps(result), "x")

  # https://github.com/tidyverse/dbplyr/issues/928
  result <- lazy_frame(cyl = 1, vs = 1) |>
    rename(vs = cyl, new_vs = vs) |>
    group_by(vs)
  expect_equal(op_grps(result), "vs")
})
