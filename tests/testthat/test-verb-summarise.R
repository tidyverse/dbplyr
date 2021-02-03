test_that("summarise peels off a single layer of grouping", {
  mf1 <- memdb_frame(x = 1, y = 1, z = 2) %>% group_by(x, y)
  mf2 <- mf1 %>% summarise(n = n())
  expect_equal(group_vars(mf2), "x")

  mf3 <- mf2 %>% summarise(n = n())
  expect_equal(group_vars(mf3), character())
})

test_that("summarise performs partial evaluation", {
  mf1 <- memdb_frame(x = 1)

  val <- 1
  mf2 <- mf1 %>% summarise(y = x == val) %>% collect()

  expect_equal(mf2$y, 1)
})

test_that("can't refer to freshly created variables", {
  mf1 <- lazy_frame(x = 1)
  expect_error(
    summarise(mf1, y = sum(x), z = sum(y)),
    "refers to a variable"
  )
})

test_that("summarise(.groups=)", {
  df <- lazy_frame(x = 1, y = 2) %>% group_by(x, y)

  # the `dplyr::` prefix is needed for `check()`
  # should produce a message when called directly by user
  expect_message(eval_bare(
    expr(lazy_frame(x = 1, y = 2) %>% dplyr::group_by(x, y) %>% dplyr::summarise() %>% remote_query()),
    env(global_env())
  ))
  expect_snapshot(eval_bare(
    expr(lazy_frame(x = 1, y = 2) %>% dplyr::group_by(x, y) %>% dplyr::summarise() %>% remote_query()),
    env(global_env())
  ))

  # should be silent when called in another package
  expect_silent(eval_bare(
    expr(lazy_frame(x = 1, y = 2) %>% dplyr::group_by(x, y) %>% dplyr::summarise() %>% remote_query()),
    asNamespace("testthat")
  ))

  expect_equal(df %>% summarise() %>% group_vars(), "x")
  expect_equal(df %>% summarise(.groups = "drop_last") %>% group_vars(), "x")
  expect_equal(df %>% summarise(.groups = "drop") %>% group_vars(), character())
  expect_equal(df %>% summarise(.groups = "keep") %>% group_vars(), c("x", "y"))

  expect_snapshot_error(df %>% summarise(.groups = "rowwise"))
})

# sql-render --------------------------------------------------------------

test_that("quoting for rendering summarized grouped table", {
  out <- memdb_frame(x = 1, .name = "verb-summarise") %>%
    group_by(x) %>%
    summarise(n = n())
  expect_snapshot(out %>% sql_render)
  expect_equal(out %>% collect, tibble(x = 1, n = 1L))
})

# sql-build ---------------------------------------------------------------

test_that("summarise generates group_by and select", {
  out <- lazy_frame(g = 1) %>%
    group_by(g) %>%
    summarise(n = n()) %>%
    sql_build()

  expect_equal(out$group_by, sql('`g`'))
  expect_equal(out$select, sql('`g`', 'COUNT(*) AS `n`'))
})


# ops ---------------------------------------------------------------------

test_that("summarise replaces existing", {
  out <- tibble(x = 1, y = 2) %>% tbl_lazy() %>% summarise(z = 1)
  expect_equal(op_vars(out), "z")
})

test_that("summarised vars are always named", {
  mf <- dbplyr::memdb_frame(a = 1)

  out1 <- mf %>% summarise(1) %>% op_vars()
  expect_equal(out1, "1")
})

test_that("grouped summary keeps groups", {
  out <- tibble(g = 1, x = 1) %>%
    tbl_lazy() %>%
    group_by(g) %>%
    summarise(y = 1)
  expect_equal(op_vars(out), c("g", "y"))
})

test_that("summarise drops one grouping level", {
  df <- tibble(g1 = 1, g2 = 2, x = 3) %>% tbl_lazy() %>% group_by(g1, g2)
  out1 <- df %>% summarise(y = 1)
  out2 <- out1 %>% summarise(y = 2)

  expect_equal(op_grps(out1), "g1")
  expect_equal(op_grps(out2), character())
})

