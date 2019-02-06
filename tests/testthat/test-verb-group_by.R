context("group_by")

test_that("group_by with add = TRUE adds groups", {
  mf <- memdb_frame(x = 1:3, y = 1:3)
  gf1 <- mf %>% group_by(x, y)
  gf2 <- mf %>% group_by(x) %>% group_by(y, add = TRUE)

  expect_equal(group_vars(gf1), c("x", "y"))
  expect_equal(group_vars(gf2), c("x", "y"))
})

test_that("collect, collapse and compute preserve grouping", {
  g <- memdb_frame(x = 1:3, y = 1:3) %>% group_by(x, y)

  expect_equal(group_vars(compute(g)), c("x", "y"))
  expect_equal(group_vars(collapse(g)), c("x", "y"))
  expect_equal(group_vars(collect(g)), c("x", "y"))
})

test_that("joins preserve grouping", {
  g <- memdb_frame(x = 1:3, y = 1:3) %>% group_by(x)

  expect_equal(group_vars(inner_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(left_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(semi_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(anti_join(g, g, by = c("x", "y"))), "x")
})

test_that("group_by can perform mutate", {
  mf <- memdb_frame(x = 3:1, y = 1:3)
  out <- mf %>%
    group_by(z = x + y) %>%
    summarise(n = n()) %>%
    collect()

  expect_equal(out, tibble(z = 4L, n = 3L))
})


# sql_build ---------------------------------------------------------------

test_that("ungroup drops PARTITION BY", {
  out <- lazy_frame(x = 1) %>%
    group_by(x) %>%
    ungroup() %>%
    mutate(x = rank(x)) %>%
    sql_build()
  expect_equal(out$select, sql(x = 'RANK() OVER (ORDER BY `x`)'))
})

# ops ---------------------------------------------------------------------

test_that("group_by overrides existing groups", {
  df <- data_frame(g1 = 1, g2 = 2, x = 3) %>% tbl_lazy()

  out1 <- df %>% group_by(g1)
  expect_equal(op_grps(out1), "g1")

  out2 <- out1 %>% group_by(g2)
  expect_equal(op_grps(out2), "g2")
})

test_that("group_by increases grouping if add = TRUE", {
  df <- data_frame(g1 = 1, g2 = 2, x = 3) %>% tbl_lazy()

  out <- df %>% group_by(g1) %>% group_by(g2, add = TRUE)
  expect_equal(op_grps(out), c("g1", "g2"))
})


test_that("ungroup drops all groups", {
  out1 <- lazy_frame(g1 = 1, g2 = 2) %>%
    group_by(g1, g2) %>%
    ungroup()

  out2 <- lazy_frame(g1 = 1, g2 = 2) %>%
    group_by(g1, g2) %>%
    ungroup() %>%
    rename(g3 = g1)

  expect_equal(op_grps(out1), character())
  expect_equal(op_grps(out2), character())
})
