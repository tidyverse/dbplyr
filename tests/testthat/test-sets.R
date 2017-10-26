context("sets")

test_that("column order is matched", {
  df1 <- memdb_frame(x = 1, y = 2)
  df2 <- memdb_frame(y = 1, x = 2)

  out <- collect(dplyr::union(df1, df2))
  expect_equal(out, tibble(x = c(1, 2), y = c(2, 1)))
})

test_that("missing columns filled with NULL", {
  df1 <- memdb_frame(x = 1)
  df2 <- memdb_frame(y = 1)

  out <- collect(dplyr::union(df1, df2))
  expect_equal(out, tibble(x = c(1, NA), y = c(NA, 1)))
})

# SQL generation ----------------------------------------------------------

test_that("union and union all work for all backends", {
  df <- tibble(x = 1:10, y = x %% 2)

  tbls_full <- test_load(df)
  tbls_filter <- test_load(filter(df, y == 0))

  tbls_full %>%
    map2(tbls_filter, union) %>%
    expect_equal_tbls()

  tbls_full %>%
    map2(tbls_filter, union_all) %>%
    expect_equal_tbls()
})

test_that("intersect and setdiff work for supported backends", {
  df <- tibble(x = 1:10, y = x %% 2)

  # MySQL doesn't support EXCEPT or INTERSECT
  tbls_full <- test_load(df, ignore = c("mysql", "MariaDB"))
  tbls_filter <- test_load(filter(df, y == 0), ignore = c("mysql", "MariaDB"))

  tbls_full %>%
    map2(tbls_filter, intersect) %>%
    expect_equal_tbls()

  tbls_full %>%
    map2(tbls_filter, setdiff) %>%
    expect_equal_tbls()
})

test_that("SQLite warns if set op attempted when tbl has LIMIT", {
  mf <- memdb_frame(x = 1:2)
  m1 <- head(mf, 1)

  expect_error(dplyr::union(mf, m1), "does not support")
  expect_error(dplyr::union(m1, mf), "does not support")
})

test_that("other backends can combine with a limit", {
  df <- tibble(x = 1:2)

  # sqlite only allows limit at top level
  tbls_full <- test_load(df, ignore = "sqlite")
  tbls_head <- lapply(test_load(df, ignore = "sqlite"), head, n = 1)

  tbls_full %>%
    map2(tbls_head, union) %>%
    expect_equal_tbls()
  tbls_full %>%
    map2(tbls_head, union_all) %>%
    expect_equal_tbls()
})
