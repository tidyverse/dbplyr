test_that("group_by with .add = TRUE adds groups", {
  mf <- memdb_frame(x = 1:3, y = 1:3)
  gf1 <- mf %>% group_by(x, y)
  gf2 <- mf %>% group_by(x) %>% group_by(y, .add = TRUE)

  expect_equal(group_vars(gf1), c("x", "y"))
  expect_equal(group_vars(gf2), c("x", "y"))
})

test_that("warns about add argument ", {
  mf <- memdb_frame(x = 1:3, y = 1:3)
  expect_warning(
    gf <- mf %>% group_by(x) %>% group_by(y, add = TRUE),
    "deprecated"
  )
  expect_equal(group_vars(gf), c("x", "y"))
})

test_that("errors for .drop = FALSE", {
  expect_snapshot(
    error = TRUE,
    lazy_frame(x = 1:3, y = 1:3) %>% group_by(y, .drop = FALSE)
  )
})

test_that("informative errors for missing variables", {
  expect_snapshot({
    (expect_error(lazy_frame(x = 1:3) %>% group_by(y)))
  })
})

test_that("group_by() produces nice error messages", {
  lf <- lazy_frame(x = 1)

  expect_snapshot(error = TRUE, {
    lf %>% group_by(z = non_existent + 1)
    lf %>% group_by(across(non_existent))
  })
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

test_that("group_by handles empty dots", {
  lf <- lazy_frame(x = 1) %>% group_by(x)

  expect_equal(lf %>% group_by() %>% group_vars(), character())
  expect_equal(lf %>% group_by(!!!list()) %>% group_vars(), character())
  expect_equal(lf %>% group_by(.add = TRUE) %>% group_vars(), c("x"))
})

test_that("can select variables via pick()", {
  lf <- lazy_frame(x_1 = 1, x_2 = 1, y = 1)
  expect_equal(
    lf %>% group_by(pick(starts_with("x_"))) %>% op_grps(),
    c("x_1", "x_2")
  )
})

test_that("only adds step if necessary", {
  lf <- lazy_frame(x = 1, y = 1)
  expect_equal(lf %>% group_by(), lf)

  expect_equal(lf %>% ungroup(), lf)
  expect_equal(lf %>% ungroup(x), lf)

  lf_grouped <- lf %>% group_by(x)
  lf_grouped2 <- lf_grouped %>% group_by(x)
  expect_equal(lf_grouped, lf_grouped2)
  expect_equal(lf_grouped %>% ungroup(y), lf_grouped)

  out <- lf_grouped %>% mutate(y = y - mean(y)) %>% group_by()
  expect_equal(group_vars(out), character())
})

test_that("group_by mutate is not optimised away", {
  lf <- lazy_frame(x = 1) %>% group_by(x)

  expect_equal(
    lf %>% group_by(x = x + 1) %>% remote_query(),
    sql("SELECT `x` + 1.0 AS `x`\nFROM `df`")
  )
})


# sql_build ---------------------------------------------------------------

test_that("ungroup drops PARTITION BY", {
  suppressWarnings(check_na_rm(FALSE))

  out <- lazy_frame(x = 1) %>%
    group_by(x) %>%
    ungroup() %>%
    mutate(x = sum(x)) %>%
    sql_build()
  expect_equal(out$select, sql(x = 'SUM(`x`) OVER ()'))
})

# ops ---------------------------------------------------------------------

test_that("group_by overrides existing groups", {
  df <- tibble(g1 = 1, g2 = 2, x = 3) %>% tbl_lazy()

  out1 <- df %>% group_by(g1)
  expect_equal(op_grps(out1), "g1")

  out2 <- out1 %>% group_by(g2)
  expect_equal(op_grps(out2), "g2")
})

test_that("group_by increases grouping if add = TRUE", {
  df <- tibble(g1 = 1, g2 = 2, x = 3) %>% tbl_lazy()

  out <- df %>% group_by(g1) %>% group_by(g2, .add = TRUE)
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

test_that("ungroup() produces nice error messages", {
  expect_snapshot(error = TRUE, {
    lazy_frame(x = 1) %>% ungroup(non_existent)
  })
})
