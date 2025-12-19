test_that("mutate computed before summarise", {
  mf <- local_memdb_frame(x = c(1, 2, 3), y = c(9, 8, 7))

  out <- mutate(mf, z = x + y) |>
    summarise(sum_z = sum(z, na.rm = TRUE)) |>
    collect()

  expect_equal(out$sum_z, 30)
})

test_that("correctly inlines across all verbs", {
  lf <- lazy_frame(x = 1, y = 2)

  # single table verbs
  expect_selects(lf |> arrange(x) |> mutate(z = 1), 1)
  expect_selects(lf |> distinct() |> mutate(z = 1), 2)
  expect_selects(lf |> filter(x == 1) |> mutate(z = 1), 1)
  expect_selects(lf |> head(1) |> mutate(z = 1), 1)
  expect_selects(lf |> mutate(z = x + 1) |> mutate(z = 1), 2)
  expect_selects(lf |> select(y = x) |> mutate(z = 1), 2)
  expect_selects(lf |> summarise(y = mean(x)) |> mutate(z = 1), 2)

  # two table verbs
  lf2 <- lazy_frame(x = 1)
  expect_selects(lf |> left_join(lf2, by = "x") |> mutate(z = 1), 2)
  expect_selects(lf |> semi_join(lf2, by = "x") |> mutate(z = 1), 3)
  expect_selects(lf |> union(lf2) |> mutate(z = 1), 3)

  # Special cases
  expect_selects(lf |> mutate(x2 = x + 1, x3 = x2 + 1), 2)
  expect_selects(lf |> mutate(x) |> mutate(y = 1), 1)
})

test_that("mutate() isn't inlined after distinct() #1119", {
  mf <- local_memdb_frame(x = 1:2)
  expect_equal(
    mf |>
      distinct(x) |>
      mutate(x = 0) |>
      collect(),
    tibble(x = c(0, 0))
  )

  lf <- lazy_frame(x = 1:2)
  expect_snapshot(
    lf |>
      distinct(x) |>
      mutate(x = 0)
  )
})

test_that("can use window function after summarise and pure projection #1104", {
  lf <- lazy_frame(g = 1, x = 1) |>
    group_by(g) |>
    summarise(x = 1) |>
    select(g)

  expect_snapshot({
    (expect_no_error(lf |> mutate(r = row_number())))
  })
})

test_that("can refer to fresly created values", {
  out1 <- local_memdb_frame(x1 = 1) |>
    mutate(x2 = x1 + 1, x3 = x2 + 1, x4 = x3 + 1) |>
    collect()
  expect_equal(out1, tibble(x1 = 1, x2 = 2, x3 = 3, x4 = 4))

  out2 <- copy_to(
    test_sqlite(),
    tibble(x = 1),
    name = "multi_mutate",
    overwrite = TRUE
  ) |>
    mutate(x = x + 1, x = x + 2, x = x + 4)
  expect_equal(collect(out2), tibble(x = 8))
  expect_snapshot(show_query(out2))
})

test_that("transmute includes all needed variables", {
  lf <- lazy_frame(x = 1, y = 2)
  out <- transmute(lf, x = x / 2, x2 = x + y)
  expect_equal(op_vars(out$lazy_query$x), c("x", "y"))
  expect_snapshot(out)
})

test_that("queries are not nested unnecessarily", {
  # Should only be one query deep
  sql <- local_memdb_frame(x = 1) |>
    mutate(y = x + 1, a = y + 1, b = y + 1) |>
    sql_build()

  expect_s3_class(sql$from, "select_query")
  expect_s3_class(sql$from$from, "base_query")
})

test_that("maintains order of existing columns (#3216, #3223)", {
  lazy <- lazy_frame(x = 1, y = 2) |>
    mutate(z = 3, y = 4, y = 5)

  expect_equal(op_vars(lazy), c("x", "y", "z"))
})

test_that("supports overwriting variables (#3222)", {
  df <- local_memdb_frame(x = 1, y = 2) |>
    mutate(y = 4, y = 5) |>
    collect()
  expect_equal(df, tibble(x = 1, y = 5))

  df <- local_memdb_frame(x = 1, y = 2) |>
    mutate(y = 4, y = y + 1) |>
    collect()
  expect_equal(df, tibble(x = 1, y = 5))

  df <- local_memdb_frame(x = 1, y = 2) |>
    mutate(y = 4, y = x + 4) |>
    collect()
  expect_equal(df, tibble(x = 1, y = 5))
})

test_that("across() does not select grouping variables", {
  df <- lazy_frame(g = 1, x = 1)

  # SELECT `g`, 0.0 AS `x`
  expect_snapshot(df |> group_by(g) |> mutate(across(.fns = ~0)))
  expect_snapshot(df |> group_by(g) |> transmute(across(.fns = ~0)))
})

test_that("transmute() keeps grouping variables", {
  lf <- lazy_frame(g = 1, x = 1) |> group_by(g)
  expect_equal(lf |> transmute(x = 1) |> op_vars(), c("g", "x"))
  expect_equal(lf |> transmute(x = 1, g) |> op_vars(), c("x", "g"))
})

test_that("across() can access previously created variables", {
  out <- local_memdb_frame(x = 1) |> mutate(y = 2, across(y, sqrt))
  lf <- lazy_frame(x = 1) |> mutate(y = 2, across(y, sqrt))

  expect_equal(
    collect(out),
    tibble(x = 1, y = sqrt(2))
  )

  expect_equal(
    op_vars(out),
    c("x", "y")
  )

  expect_snapshot(remote_query(lf))
})

test_that("across() uses original column rather than overridden one", {
  db <- local_memdb_frame(x = 2, y = 4, z = 6)
  expect_equal(
    db |> mutate(across(everything(), ~ .x / x)) |> collect(),
    tibble(x = 1, y = 2, z = 3)
  )
  expect_equal(
    db |>
      mutate(
        x = -x,
        across(everything(), ~ .x / x),
        y = y + x
      ) |>
      collect(),
    tibble(x = 1, y = -1, z = -3)
  )

  lf <- lazy_frame(x = 2, y = 4, z = 6)
  expect_equal(
    lf |>
      mutate(across(everything(), ~ .x / x)) |>
      remote_query(),
    sql(
      "SELECT \"x\" / \"x\" AS \"x\", \"y\" / \"x\" AS \"y\", \"z\" / \"x\" AS \"z\"\nFROM \"df\""
    )
  )
  expect_snapshot(
    lf |>
      mutate(
        x = -x,
        across(everything(), ~ .x / x),
        y = y + x
      )
  )
})

test_that("new columns take precedence over global variables", {
  y <- "global var"
  db <- local_memdb_frame(x = 1) |> mutate(y = 2, z = y + 1)
  lf <- lazy_frame(x = 1) |> mutate(y = 2, z = y + 1)

  expect_equal(
    collect(db),
    tibble(x = 1, y = 2, z = 3)
  )

  expect_snapshot(remote_query(lf))
})

test_that("constants do not need a new query", {
  expect_equal(
    lazy_frame(x = 1, y = 2) |> mutate(z = 2, z = 3) |> remote_query(),
    sql("SELECT \"df\".*, 3.0 AS \"z\"\nFROM \"df\"")
  )
})

test_that("mutate() produces nice error messages", {
  options(lifecycle_verbosity = "quiet")
  expect_snapshot(error = TRUE, {
    lazy_frame(x = 1) |> mutate(z = non_existent + 1)

    # `...` cannot be evaluated
    lazy_frame(x = 1) |> mutate(across(x, mean, na.rm = z))

    # `.fns` cannot be evaluated
    lazy_frame(x = 1) |> mutate(across(x, .fns = "a"))
  })
})

test_that("empty mutate returns input", {
  df <- lazy_frame(x = 1)
  gf <- group_by(df, x)

  expect_equal(mutate(df), df)
  expect_equal(mutate(df, .by = x), df)
  expect_equal(mutate(gf), gf)

  expect_equal(mutate(df, !!!list()), df)
  expect_equal(mutate(df, !!!list(), .by = x), df)
  expect_equal(mutate(gf, !!!list()), gf)
})

test_that("can use .sql pronoun", {
  lf <- lazy_frame(x = 1)

  # Similar capability to existing escaping mechanisms, but more
  # convenient for package usage
  expect_equal(
    lf |> mutate(y = .sql$foo(x), z = .sql$abc + x),
    lf |> mutate(y = foo(x), z = remote(abc) + x)
  )
})

# .by -------------------------------------------------------------------------

test_that("can group transiently using `.by`", {
  df <- local_memdb_frame(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))

  out <- mutate(df, x = mean(x), .by = g) |>
    arrange(g) |>
    collect()

  expect_identical(out$g, c(1, 1, 1, 2, 2))
  expect_identical(out$x, c(3, 3, 3, 2, 2))
  expect_equal(group_vars(out), character())
})

test_that("can `NULL` out the `.by` column", {
  df <- lazy_frame(x = 1:3, y = 1:3)
  out <- mutate(df, x = NULL, .by = x)

  expect_identical(op_vars(out), "y")
  expect_identical(remote_query(out), sql("SELECT \"y\"\nFROM \"df\""))
})

# .order, .frame -----------------------------------------------------------

test_that("can order transiently using `.order`", {
  df <- local_memdb_frame(g = c(1, 1, 2, 2), x = c(1, 2, 3, 4))
  df2 <- df |> mutate(r = cumsum(x), .by = g, .order = x)
  out <- df2 |> collect()

  expect_equal(op_sort(df2), NULL)
  expect_equal(out$r, c(1, 3, 3, 7))
})

test_that("can specify window frame using `.frame`", {
  df <- local_memdb_frame(g = c(1, 1, 1), x = c(1.0, 2.0, 3.0))
  df2 <- df |> mutate(r = sum(x), .by = g, .order = x, .frame = c(-Inf, 0))
  out <- df2 |> collect()

  expect_equal(op_frame(df2), NULL)
  expect_equal(out$r, c(1, 3, 6))
})

test_that(".order and .frame are scoped to mutate call", {
  lf <- lazy_frame(g = 1, x = 1, y = 1)

  # The second mutate should not have order/frame from the first
  out <- lf |>
    mutate(r1 = sum(x), .by = g, .order = y, .frame = c(-Inf, 0)) |>
    mutate(r2 = mean(x), .by = g)

  expect_snapshot(out)
})

test_that(".order generates correct SQL", {
  lf <- lazy_frame(g = 1, x = 1, y = 1)

  expect_snapshot({
    lf |> mutate(r = cumsum(x), .by = g, .order = y)
    lf |> mutate(r = cumsum(x), .by = g, .order = c(x, desc(y)))
  })
})

test_that(".frame generates correct SQL", {
  lf <- lazy_frame(g = 1, x = 1, y = 1)

  expect_snapshot({
    lf |> mutate(r = sum(x), .by = g, .order = y, .frame = c(-Inf, 0))
    lf |> mutate(r = sum(x), .by = g, .order = y, .frame = c(-1, 1))
  })
})

test_that(".order validates inputs", {
  lf <- lazy_frame(x = 1, y = 1)

  expect_snapshot(error = TRUE, {
    lf |> mutate(r = cumsum(x), .order = x + y)
    lf |> mutate(r = cumsum(x), .order = foo())
  })
})

test_that(".frame validates inputs", {
  lf <- lazy_frame(x = 1)

  expect_snapshot(error = TRUE, {
    lf |> mutate(r = sum(x), .frame = c(1, 2, 3))
    lf |> mutate(r = sum(x), .frame = c("a", "b"))
  })
})

# SQL generation -----------------------------------------------------------

test_that("mutate generates new variables and replaces existing", {
  df1 <- local_memdb_frame(x = 1)
  out <- df1 |> mutate(x = 2, y = 3) |> collect()
  expect_equal(out, tibble(x = 2, y = 3))
})

test_that("transmute only returns new variables", {
  df1 <- local_memdb_frame(x = 1)
  out <- df1 |> transmute(y = 3) |> collect()
  expect_equal(out, tibble(y = 3))
})

test_that("mutate calls windowed versions of sql functions", {
  df1 <- local_memdb_frame(x = 1:4, g = rep(c(1, 2), each = 2))
  out <- df1 |> group_by(g) |> mutate(r = row_number(x)) |> collect()
  expect_equal(out$r, c(1, 2, 1, 2))
})

test_that("recycled aggregates generate window function", {
  df1 <- local_memdb_frame(
    x = as.numeric(1:4),
    g = rep(c(1, 2), each = 2)
  )
  out <- df1 |>
    group_by(g) |>
    mutate(r = x - mean(x, na.rm = TRUE)) |>
    collect()

  expect_equal(out$r, c(-0.5, 0.5, -0.5, 0.5))
})

test_that("cumulative aggregates generate window function", {
  df1 <- local_memdb_frame(x = 1:4, g = rep(c(1, 2), each = 2))
  out <- df1 |>
    group_by(g) |>
    arrange(x) |>
    mutate(r = cumsum(x)) |>
    collect()

  expect_equal(out$r, c(1, 3, 3, 7))
})

test_that("mutate overwrites previous variables", {
  df <- local_memdb_frame(x = 1:5) |>
    mutate(x = x + 1) |>
    mutate(x = x + 1) |>
    collect()

  expect_equal(names(df), "x")
  expect_equal(df$x, 1:5 + 2)
})

test_that("sequence of operations work", {
  out <- local_memdb_frame(x = c(1, 2, 3, 4)) |>
    select(y = x) |>
    mutate(z = 2 * y) |>
    filter(z == 2) |>
    collect()

  expect_equal(out, tibble(y = 1, z = 2))
})

# .before, .after, .keep ------------------------------------------------------

test_that(".keep = 'unused' keeps variables explicitly mentioned", {
  df <- lazy_frame(x = 1, y = 2)
  out <- mutate(df, x1 = x + 1, y = y, .keep = "unused")
  expect_equal(op_vars(out), c("y", "x1"))
})

test_that(".keep = 'used' not affected by across()", {
  df <- lazy_frame(x = 1, y = 2, z = 3, a = "a", b = "b", c = "c")

  # This must evaluate every column in order to figure out if should
  # be included in the set or not, but that shouldn't be counted for
  # the purposes of "used" variables
  out <- mutate(df, across(c("x", "y", "z"), identity), .keep = "unused")
  expect_equal(op_vars(out), op_vars(df))
})

test_that(".keep = 'used' keeps variables used in expressions", {
  df <- lazy_frame(a = 1, b = 2, c = 3, x = 1, y = 2)
  out <- mutate(df, xy = x + y, .keep = "used")
  expect_equal(op_vars(out), c("x", "y", "xy"))
})

test_that(".keep = 'none' only keeps grouping variables", {
  df <- lazy_frame(x = 1, y = 2)
  gf <- group_by(df, x)

  expect_equal(op_vars(mutate(df, z = 1, .keep = "none")), "z")
  expect_equal(op_vars(mutate(gf, z = 1, .keep = "none")), c("x", "z"))
})

test_that(".keep = 'none' retains original ordering (#5967)", {
  df <- lazy_frame(x = 1, y = 2)
  expect_equal(
    df |> mutate(y = 1, x = 2, .keep = "none") |> op_vars(),
    c("x", "y")
  )

  # even when grouped
  gf <- group_by(df, x)
  expect_equal(
    gf |> mutate(y = 1, x = 2, .keep = "none") |> op_vars(),
    c("x", "y")
  )
})

test_that("can use .before and .after to control column position", {
  df <- lazy_frame(x = 1, y = 2)
  expect_equal(mutate(df, z = 1) |> op_vars(), c("x", "y", "z"))
  expect_equal(mutate(df, z = 1, .before = 1) |> op_vars(), c("z", "x", "y"))
  expect_equal(mutate(df, z = 1, .after = 1) |> op_vars(), c("x", "z", "y"))

  # but doesn't affect order of existing columns
  df <- lazy_frame(x = 1, y = 2)
  expect_equal(mutate(df, x = 1, .after = y) |> op_vars(), c("x", "y"))
})

test_that(".keep and .before/.after interact correctly", {
  df <- lazy_frame(x = 1, y = 1, z = 1, a = 1, b = 2, c = 3) |>
    group_by(a, b)

  expect_equal(
    mutate(df, d = 1, x = 2, .keep = "none") |> op_vars(),
    c("x", "a", "b", "d")
  )
  expect_equal(
    mutate(df, d = 1, x = 2, .keep = "none", .before = "a") |> op_vars(),
    c("x", "d", "a", "b")
  )
  expect_equal(
    mutate(df, d = 1, x = 2, .keep = "none", .after = "a") |> op_vars(),
    c("x", "a", "d", "b")
  )
})

test_that("dropping column with `NULL` then readding it retains original location", {
  df <- lazy_frame(x = 1, y = 2, z = 3, a = 4)
  df <- group_by(df, z)

  expect_equal(
    mutate(df, y = NULL, y = 3, .keep = "all") |> op_vars(),
    c("x", "y", "z", "a")
  )
  expect_equal(
    mutate(df, b = a, y = NULL, y = 3, .keep = "used") |> op_vars(),
    c("y", "z", "a", "b")
  )
  expect_equal(
    mutate(df, b = a, y = NULL, y = 3, .keep = "unused") |> op_vars(),
    c("x", "y", "z", "b")
  )

  # It isn't treated as a "new" column
  expect_equal(
    mutate(df, y = NULL, y = 3, .keep = "all", .before = x) |> op_vars(),
    c("x", "y", "z", "a")
  )
})

test_that(".keep= always retains grouping variables (#5582)", {
  df <- lazy_frame(x = 1, y = 2, z = 3) |> group_by(z)
  expect_equal(
    df |> mutate(a = x + 1, .keep = "none") |> op_vars(),
    c("z", "a")
  )
  expect_equal(
    df |> mutate(a = x + 1, .keep = "all") |> op_vars(),
    c("x", "y", "z", "a")
  )
  expect_equal(
    df |> mutate(a = x + 1, .keep = "used") |> op_vars(),
    c("x", "z", "a")
  )
  expect_equal(
    df |> mutate(a = x + 1, .keep = "unused") |> op_vars(),
    c("y", "z", "a")
  )
})


# sql_render --------------------------------------------------------------

test_that("quoting for rendering mutated grouped table", {
  out <- local_memdb_frame(x = 1, y = 2) |> mutate(y = x)
  expect_match(out |> sql_render(), "^SELECT `x`, `x` AS `y`\nFROM `[^`]*`$")
  expect_equal(out |> collect(), tibble(x = 1, y = 1))
})

test_that("mutate generates subqueries as needed", {
  lf <- lazy_frame(x = 1, con = simulate_sqlite())

  expect_snapshot(lf |> mutate(x = x + 1, x = x + 1))
  expect_snapshot(lf |> mutate(x1 = x + 1, x2 = x1 + 1))
})

test_that("mutate collapses over nested select", {
  lf <- lazy_frame(g = 0, x = 1, y = 2)

  expect_snapshot(lf |> select(x:y) |> mutate(x = x * 2, y = y * 2))
  expect_snapshot(lf |> select(y:x) |> mutate(x = x * 2, y = y * 2))
})

test_that("mutate() uses star", {
  lf <- lazy_frame(x = 1, y = 1)

  expect_equal(
    lf |> mutate(z = 1L) |> remote_query(),
    sql("SELECT \"df\".*, 1 AS \"z\"\nFROM \"df\"")
  )

  expect_equal(
    lf |> mutate(a = 1L, .before = 1) |> remote_query(),
    sql("SELECT 1 AS \"a\", \"df\".*\nFROM \"df\"")
  )

  expect_equal(
    lf |> transmute(a = 1L, x, y, z = 2L) |> remote_query(),
    sql("SELECT 1 AS \"a\", \"df\".*, 2 AS \"z\"\nFROM \"df\"")
  )

  # does not use * if `use_star = FALSE`
  expect_equal(
    lf |>
      mutate(z = 1L) |>
      remote_query(sql_options = sql_options(use_star = FALSE)),
    sql("SELECT \"x\", \"y\", 1 AS \"z\"\nFROM \"df\"")
  )
})

# sql_build ---------------------------------------------------------------

test_that("mutate generates simple expressions", {
  out <- lazy_frame(x = 1) |>
    mutate(y = x + 1L) |>
    sql_build()

  expect_equal(out$select, sql('"df".*', y = '"x" + 1'))
})

test_that("mutate can drop variables with NULL", {
  out <- lazy_frame(x = 1, y = 1) |>
    mutate(y = NULL)

  expect_named(sql_build(out)$select, "x")
  expect_equal(op_vars(out), "x")
})

test_that("var = NULL works when var is in original data", {
  lf <- lazy_frame(x = 1) |> mutate(x = 2, z = x * 2, x = NULL)
  expect_equal(sql_build(lf)$select, sql(z = "\"x\" * 2.0"))
  expect_equal(op_vars(lf), "z")
  expect_snapshot(remote_query(lf))
})

test_that("var = NULL when var is in final output", {
  lf <- lazy_frame(x = 1) |> mutate(y = NULL, y = 3)
  expect_equal(sql_build(lf)$select, sql("\"df\".*", y = "3.0"))
  expect_equal(op_vars(lf), c("x", "y"))
  expect_snapshot(remote_query(lf))
})

test_that("temp var with nested arguments", {
  lf <- lazy_frame(x = 1) |> mutate(y = 2, z = y * 2, y = NULL)
  expect_equal(sql_build(lf)$select, sql(x = "\"x\"", z = "\"y\" * 2.0"))
  expect_equal(op_vars(lf), c("x", "z"))
  expect_snapshot(remote_query(lf))
})

test_that("mutate_all generates correct sql", {
  out <- lazy_frame(x = 1, y = 1) |>
    dplyr::mutate_all(~ . + 1L) |>
    sql_build()

  expect_equal(out$select, sql(x = '"x" + 1', y = '"y" + 1'))

  out <- lazy_frame(x = 1) |>
    dplyr::mutate_all(list(one = ~ . + 1L, two = ~ . + 2L)) |>
    sql_build()
  expect_equal(out$select, sql('"df".*', one = '"x" + 1', two = '"x" + 2'))
})

test_that("mutate_all scopes nested quosures correctly", {
  num <- 10L
  out <- lazy_frame(x = 1, y = 1) |>
    dplyr::mutate_all(~ . + num) |>
    sql_build()

  expect_equal(out$select, sql(x = '"x" + 10', y = '"y" + 10'))
})


# ops ---------------------------------------------------------------------

test_that("mutate adds new", {
  out <- tibble(x = 1) |> tbl_lazy() |> mutate(y = x + 1, z = y + 1)
  expect_equal(op_vars(out), c("x", "y", "z"))
})

test_that("mutated vars are always named", {
  mf <- local_memdb_frame(a = 1)

  out2 <- mf |> mutate(1) |> op_vars()
  expect_equal(out2, c("a", "1"))
})
