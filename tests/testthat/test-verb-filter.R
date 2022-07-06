test_that("filter captures local variables", {
  mf <- memdb_frame(x = 1:5, y = 5:1)

  z <- 3
  df1 <- mf %>% filter(x > z) %>% collect()
  df2 <- mf %>% collect() %>% filter(x > z)

  expect_equal_tbl(df1, df2)
})

test_that("two filters equivalent to one", {
  mf <- memdb_frame(x = 1:5, y = 5:1)

  df1 <- mf %>% filter(x > 3) %>% filter(y < 3)
  df2 <- mf %>% filter(x > 3, y < 3)
  expect_equal_tbl(df1, df2)
})


test_that("each argument gets implicit parens", {
  mf <- memdb_frame(
    v1 = c("a", "b", "a", "b"),
    v2 = c("b", "a", "a", "b"),
    v3 = c("a", "b", "c", "d")
  )

  mf1 <- mf %>% filter((v1 == "a" | v2 == "a") & v3 == "a")
  mf2 <- mf %>% filter(v1 == "a" | v2 == "a", v3 == "a")
  expect_equal_tbl(mf1, mf2)
})

test_that("only add step if necessary", {
  lf <- lazy_frame(x = 1:3, y = 1:3)
  expect_equal(lf %>% filter(), lf)
})

test_that("errors for named input", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(error = TRUE, filter(lf, x = 1))
  expect_snapshot(error = TRUE, filter(lf, y > 1, x = 1))
})

test_that(".preserve is not supported", {
  lf <- lazy_frame(x = 1:3, y = 1:3)
  expect_snapshot(error = TRUE, lf %>% filter(x == 1, .preserve = TRUE))
})


# SQL generation --------------------------------------------------------

test_that("filter calls windowed versions of sql functions", {
  df1 <- memdb_frame(x = 1:10, g = rep(c(1, 2), each = 5))

  out <- df1 %>% group_by(g) %>% filter(dplyr::row_number(x) < 3) %>% collect()
  expect_equal(out$x, c(1L, 2L, 6L, 7L))
})

test_that("recycled aggregates generate window function", {
  df1 <- memdb_frame(x = 1:10, g = rep(c(1, 2), each = 5))

  out <- df1 %>%
    group_by(g) %>%
    filter(x > mean(x, na.rm = TRUE)) %>%
    collect()
  expect_equal(out$x, c(4L, 5L, 9L, 10L))
})

test_that("cumulative aggregates generate window function", {
  df1 <- memdb_frame(x = c(1:3, 2:4), g = rep(c(1, 2), each = 3))
  out <- df1 %>%
    group_by(g) %>%
    window_order(x) %>%
    filter(cumsum(x) > 3)

  expect_equal(pull(out, x), c(3L, 3L, 4L))
})

# sql_build ---------------------------------------------------------------

test_that("filter generates simple expressions", {
  out <- lazy_frame(x = 1) %>%
    filter(x > 1L) %>%
    sql_build()

  expect_equal(out$where, sql('`x` > 1'))
})


# lazy_select_query -------------------------------------------------------

test_that("generates correct lazy_select_query", {
  lf <- lazy_frame(x = 1:3, y = 3:1)

  expect_equal(
    lf %>%
      filter(x > 1) %>%
      .$lazy_query,
    lazy_select_query(
      x = lf$lazy_query,
      last_op = "filter",
      select = syms(set_names(colnames(lf))),
      where = unclass(quos(x > 1))
    ),
    ignore_formula_env = TRUE
  )

  out <- lf %>%
    filter(mean(x, na.rm = TRUE) > 1) %>%
    .$lazy_query

  expect_equal(
    out,
    lazy_select_query(
      x = out$x,
      last_op = "filter",
      select = syms(set_names(colnames(lf))),
      where = set_names(list(expr(q01 > 1)), "")
    ),
    ignore_formula_env = TRUE
  )

  expect_equal(
    out$x,
    lazy_select_query(
      x = lf$lazy_query,
      last_op = "mutate",
      select = list(x = sym("x"), y = sym("y"), q01 = quo(mean(x, na.rm = TRUE)))
    ),
    ignore_formula_env = TRUE
  )
})
