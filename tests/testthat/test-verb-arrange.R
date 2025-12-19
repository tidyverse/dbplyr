test_that("two arranges equivalent to one", {
  mf <- local_memdb_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  mf1 <- mf |> arrange(x, y)
  mf2 <- mf |> arrange(y) |> arrange(x)

  expect_equal(mf1 |> collect(), mf2 |> collect())
})

test_that("correctly inlines across all verbs", {
  lf <- lazy_frame(x = 1, y = 2)

  # single table verbs
  expect_selects(lf |> arrange(x) |> arrange(y), 1)
  expect_selects(lf |> distinct() |> arrange(y), 1)
  expect_selects(lf |> filter(x) |> arrange(y), 1)
  expect_selects(lf |> head(1) |> arrange(y), 2) # TODO: 1?
  expect_selects(lf |> mutate(y = x + 1) |> arrange(y), 1)
  expect_selects(lf |> select(y = x) |> arrange(y), 1)
  expect_selects(lf |> summarise(y = mean(x)) |> arrange(y), 1)

  # two table verbs
  lf2 <- lazy_frame(x = 1)
  expect_selects(lf |> left_join(lf2, by = "x") |> arrange(y), 2)
  expect_selects(lf |> right_join(lf2, by = "x") |> arrange(y), 2)
  expect_selects(lf |> semi_join(lf2, by = "x") |> arrange(y), 3)
  expect_selects(lf |> union(lf2) |> arrange(y), 3)
})

# sql_render --------------------------------------------------------------

test_that("quoting for rendering ordered grouped table", {
  db <- copy_to(
    test_sqlite(),
    tibble(x = 1, y = 2),
    name = "test-verb-arrange",
    overwrite = TRUE
  )
  out <- db |> group_by(x) |> arrange(y) |> ungroup()
  expect_snapshot(sql_render(out))
  expect_equal(collect(out), tibble(x = 1, y = 2))
})

test_that("arrange renders correctly (#373)", {
  expect_snapshot({
    "# arrange renders correctly"
    lf <- lazy_frame(a = 1:3, b = 3:1)

    "basic"
    lf |> arrange(a)

    "double arrange"
    lf |> arrange(a) |> arrange(b)

    "remove ordered by"
    lf |> arrange(a) |> select(-a)
    lf |> arrange(a) |> select(-a) |> arrange(b)

    "un-arrange"
    lf |> arrange(a) |> arrange()
    lf |> arrange(a) |> select(-a) |> arrange()

    "use order"
    lf |> arrange(a) |> select(-a) |> mutate(c = lag(b))
  })
})

test_that("arrange renders correctly for single-table verbs (#373)", {
  expect_snapshot({
    lf <- lazy_frame(a = 1:3, b = 3:1)

    "head"
    lf |> head(1) |> arrange(a)
    lf |> arrange(a) |> head(1)
    lf |> arrange(a) |> head(1) |> arrange(b)

    "mutate"
    lf |> mutate(a = b) |> arrange(a)

    "complex mutate"
    lf |> arrange(a) |> mutate(a = b) |> arrange(a)
    lf |> arrange(a) |> mutate(a = 1) |> arrange(b)
    lf |> mutate(a = -a) |> arrange(a) |> mutate(a = -a)
  })
})

test_that("can combine arrange with dual table verbs", {
  expect_snapshot({
    lf <- lazy_frame(a = 1:3, b = 3:1)
    rf <- lazy_frame(a = 1:3, c = 4:6)

    "warn if arrange before join"
    lf |> arrange(a) |> left_join(rf)
    lf |> arrange(a) |> semi_join(rf)
    lf |> arrange(a) |> union(rf)

    "can arrange after join"
    lf |> left_join(rf) |> arrange(a)
    lf |> semi_join(rf) |> arrange(a)
    lf |> union(rf) |> arrange(a)
  })
})

test_that("only add step if necessary", {
  lf <- lazy_frame(x = 1:3, y = 1:3)
  expect_equal(lf |> arrange(), lf)
})

# sql_build ---------------------------------------------------------------

test_that("arrange generates order_by", {
  out <- lazy_frame(x = 1, y = 1) |>
    arrange(x) |>
    sql_build()

  expect_equal(out$order_by, sql('"x"'))
})

test_that("arrange converts desc", {
  out <- lazy_frame(x = 1, y = 1) |>
    arrange(desc(x)) |>
    sql_build()

  expect_equal(out$order_by, sql('"x" DESC'))
})

test_that("grouped arrange doesn't order by groups", {
  out <- lazy_frame(x = 1, y = 1) |>
    group_by(x) |>
    arrange(y) |>
    sql_build()

  expect_equal(out$order_by, sql('"y"'))
})

test_that("grouped arrange order by groups when .by_group  is set to TRUE", {
  lf <- lazy_frame(x = 1, y = 1, con = simulate_dbi())
  out <- lf |>
    group_by(x) |>
    arrange(y, .by_group = TRUE) |>
    sql_build()
  expect_equal(out$order_by, sql(c('"x"', '"y"')))
})

# ops ---------------------------------------------------------------------

test_that("arranges captures DESC", {
  out <- lazy_frame(x = 1:3, y = 3:1) |> arrange(desc(x))

  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(desc(x))))
})

test_that("multiple arranges combine", {
  out <- lazy_frame(x = 1:3, y = 3:1) |> arrange(x) |> arrange(y)

  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y), quote(x)))
})
