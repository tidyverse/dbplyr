test_that("collect equivalent to as.data.frame/as_tibble", {
  mf <- memdb_frame(letters)

  expect_equal(as.data.frame(mf), data.frame(letters, stringsAsFactors = FALSE))
  expect_equal(as_tibble(mf), tibble(letters))
  expect_equal(collect(mf), tibble(letters))
})

test_that("explicit collection returns all data", {
  n <- 1e5 + 10 # previous default was 1e5
  big <- memdb_frame(x = seq_len(n))

  nrow1 <- big %>% as.data.frame() %>% nrow()
  nrow2 <- big %>% as_tibble() %>% nrow()
  nrow3 <- big %>% collect() %>% nrow()

  expect_equal(nrow1, n)
  expect_equal(nrow2, n)
  expect_equal(nrow3, n)
})

test_that("compute doesn't change representation", {
  mf1 <- memdb_frame(x = 5:1, y = 1:5, z = "a")
  expect_equal_tbl(mf1, mf1 %>% compute)
  expect_equal_tbl(mf1, mf1 %>% compute %>% compute)

  mf2 <- mf1 %>% mutate(z = x + y)
  expect_equal_tbl(mf2, mf2 %>% compute)
})

test_that("compute can create indexes", {
  mfs <- test_frame(x = 5:1, y = 1:5, z = 10)

  mfs %>%
    lapply(. %>% compute(indexes = c("x", "y"))) %>%
    expect_equal_tbls()

  mfs %>%
    lapply(. %>% compute(indexes = list("x", "y", c("x", "y")))) %>%
    expect_equal_tbls()

  mfs %>%
    lapply(. %>% compute(indexes = "x", unique_indexes = "y")) %>%
    expect_equal_tbls()

  mfs %>%
    lapply(. %>% compute(unique_indexes = list(c("x", "z"), c("y", "z")))) %>%
    expect_equal_tbls()
})

test_that("unique index fails if values are duplicated", {
  mfs <- test_frame(x = 5:1, y = "a", ignore = "df")
  lapply(mfs, function(.) expect_error(compute(., unique_indexes = "y")))
})

test_that("compute creates correct column names", {
  out <- memdb_frame(x = 1) %>%
    group_by(x) %>%
    summarise(n = n()) %>%
    compute() %>%
    collect()

  expect_equal(out, tibble(x = 1, n = 1L))
})

test_that("compute keeps window and groups", {
  out <- memdb_frame(x = 1, y = 1) %>%
    window_order(x) %>%
    group_by(x, y) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    compute()

  expect_equal(op_sort(out), list(quo(x)), ignore_formula_env = TRUE)
  expect_equal(op_grps(out), "x")
})

test_that("compute can handle named name", {
  name <- set_names(unique_subquery_name(), unique_subquery_name())
  expect_equal(
    memdb_frame(x = 1:10) %>%
      compute() %>%
      collect(),
    tibble(x = 1:10)
  )
})

test_that("compute can handle schema", {
  df <- memdb_frame(x = 1:10)
  on.exit(DBI::dbRemoveTable(remote_con(df), "db1"))

  expect_equal(
    df %>%
      compute(name = in_schema("main", "db1"), temporary = FALSE) %>%
      collect(),
    tibble(x = 1:10)
  )
})

test_that("collect() handles DBI error", {
  mf <- memdb_frame(x = 1)
  expect_snapshot(
    (expect_error(mf %>% mutate(a = sql("invalid sql")) %>% collect())),
    transform = snap_transform_dbi
  )
})

# ops ---------------------------------------------------------------------

test_that("sorting preserved across compute and collapse", {
  df1 <- memdb_frame(x = sample(10)) %>% arrange(x)

  df2 <- compute(df1)
  expect_equal(get_expr(op_sort(df2)[[1]]), quote(x))

  df3 <- collapse(df1)
  expect_equal(get_expr(op_sort(df3)[[1]]), quote(x))
})

