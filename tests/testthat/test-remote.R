test_that("remote_name returns name when it makes sense", {
  mf <- copy_to_test("sqlite", tibble(x = 5), name = "refxiudlph")

  # produces name after `group_by()`
  expect_equal(
    mf %>% group_by(x) %>% remote_name(),
    ident("refxiudlph")
  )

  # produces name after unarranging
  expect_equal(
    mf %>% arrange(x) %>% arrange() %>% remote_name(),
    ident("refxiudlph")
  )

  # produces name after compute()
  expect_false(is_null(mf %>% mutate(x = x + 1) %>% compute() %>% remote_name()))
})

test_that("remote_name returns null for computed tables", {
  mf <- copy_to_test("sqlite", tibble(x = 5, y = 1), name = "refxiudlph")
  expect_equal(remote_name(mf), ident("refxiudlph"))

  expect_null(mf %>% filter(x == 3) %>% remote_name())
  expect_null(mf %>% distinct(x) %>% remote_name())
  expect_null(mf %>% mutate(x = x + 1) %>% remote_name())
  expect_null(mf %>% select(x) %>% remote_name())
  expect_null(mf %>% relocate(y, x) %>% remote_name())
  expect_null(mf %>% head(3) %>% remote_name())

  expect_null(left_join(mf, mf, by = "x") %>% remote_name())
  lf <- lazy_frame(x = 1)
  expect_null(lf %>% remote_name())
  expect_null(lf %>% group_by(x) %>% remote_name())
})

test_that("can retrieve query, src and con metadata", {
  mf <- memdb_frame(x = 5)

  expect_s4_class(remote_con(mf), "DBIConnection")
  expect_s3_class(remote_src(mf), "src_sql")
  expect_s3_class(remote_query(mf), "sql")
  expect_type(remote_query_plan(mf), "character")
})
