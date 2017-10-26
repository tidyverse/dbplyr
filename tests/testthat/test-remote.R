context("test-remote.R")

test_that("remote_name returns null for computed tables", {
  mf <- memdb_frame(x = 5, .name = "refxiudlph")
  expect_equal(remote_name(mf), ident("refxiudlph"))

  mf2 <- mf %>% filter(x == 3)
  expect_equal(remote_name(mf2), NULL)
})

test_that("can retrieve query, src and con metadata", {
  mf <- memdb_frame(x = 5)

  expect_s4_class(remote_con(mf), "DBIConnection")
  expect_s3_class(remote_src(mf), "src_sql")
  expect_s3_class(remote_query(mf), "sql")
  expect_type(remote_query_plan(mf), "character")
})

