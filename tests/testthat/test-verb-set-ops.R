test_that("correctly inlines across all verbs", {
  lf <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, y = 2)

  # single table verbs
  expect_selects(lf |> arrange(x) |> union(lf2), 2)
  expect_selects(lf |> distinct() |> union(lf2), 2)
  expect_selects(lf |> filter(x == 1) |> union(lf2), 2)
  expect_selects(lf |> head(1) |> union(lf2), 2)
  expect_selects(lf |> mutate(z = x + 1) |> select(-z) |> union(lf2), 2)
  expect_selects(lf |> select(x, y) |> union(lf2), 2)
  expect_selects(lf |> summarise(x = mean(x), y = mean(y)) |> union(lf2), 2)

  # two table verbs
  lf3 <- lazy_frame(x = 1)
  expect_selects(lf |> left_join(lf3, by = "x") |> union(lf2), 2)
  expect_selects(lf |> right_join(lf3, by = "x") |> union(lf2), 2)
  expect_selects(lf |> semi_join(lf3, by = "x") |> union(lf2), 3)
  expect_selects(lf |> union(lf2) |> union(lf2), 3)
})

test_that("column order is matched", {
  df1 <- local_memdb_frame(x = 1, y = 2)
  df2 <- local_memdb_frame(y = 1, x = 2)

  out <- collect(union(df1, df2))
  expect_equal(out, tibble(x = c(1, 2), y = c(2, 1)))
})

test_that("missing columns filled with NULL", {
  df1 <- local_memdb_frame(x = 1)
  df2 <- local_memdb_frame(y = 2)

  out <- collect(union_all(df1, df2))
  expect_equal(out, tibble(x = c(1, NA), y = c(NA, 2)))
})

# SQL generation ----------------------------------------------------------

test_that("basic set ops work as expected", {
  db1 <- local_memdb_frame(x = c(1, 3))
  db2 <- local_memdb_frame(x = c(1, 2))

  expect_equal(sort(pull(union(db1, db2))), c(1, 2, 3))
  expect_equal(sort(pull(union_all(db1, db2))), c(1, 1, 2, 3))
  expect_equal(sort(pull(intersect(db1, db2))), 1)
  expect_equal(sort(pull(setdiff(db1, db2))), 3)
  expect_equal(sort(pull(setdiff(db2, db1))), 2)
})

test_that("basic set ops generate expected SQL", {
  db1 <- lazy_frame(x = 1, .name = "db1")
  db2 <- lazy_frame(x = 1, .name = "db2")

  expect_snapshot({
    union(db1, db2)
    union_all(db1, db2)
    intersect(db1, db2)
    setdiff(db1, db2)
  })
})

test_that("can combine multiple unions in one query", {
  lf1 <- lazy_frame(x = 1, y = 1, .name = "lf1")
  lf2 <- lazy_frame(y = 1, .name = "lf2")
  lf3 <- lazy_frame(z = 1, .name = "lf3")

  with_cte <- sql_options(cte = TRUE)
  lf_union <- lf1 |> union_all(lf2) |> union(lf3)

  expect_snapshot(lf_union |> show_query())
  # and with cte
  expect_snapshot(lf_union |> show_query(sql_options = with_cte))

  out <- lf_union |> mutate(a = x + y) |> sql_build()
  expect_equal(out$select, sql("\"q01\".*", "\"x\" + \"y\" AS \"a\""))
})

test_that("set ops correctly quote reused queries in CTEs", {
  lf <- lazy_frame(x = 1, .name = "lf") |> mutate(y = x + 1)
  expect_snapshot(
    union_all(lf, lf) |> show_query(sql_options = sql_options(cte = TRUE))
  )
})

test_that("SQLite warns if set op attempted when tbl has LIMIT", {
  mf <- local_memdb_frame(x = 1:2)
  m1 <- head(mf, 1)

  expect_error(union(mf, m1), "does not support")
  expect_error(union(m1, mf), "does not support")
})

test_that("but it can work with another backend", {
  con <- test_postgres()
  db <- local_db_table(con, tibble(x = 1:2), "db")

  out <- collect(head(union_all(db, db), 2))
  expect_equal(nrow(out), 2)
})

test_that("intersect works with copy = 'temp-table'", {
  df1 <- local_memdb_frame(x = 1:3)
  df2 <- tibble(x = 2:4)

  out <- intersect(df1, df2, copy = "temp-table") |> collect()
  expect_equal(out, tibble(x = 2:3))
})

test_that("intersect works with copy = 'inline'", {
  df1 <- local_memdb_frame(x = 1:3)
  df2 <- tibble(x = 2:4)

  out <- intersect(df1, df2, copy = "inline") |> collect()
  expect_equal(out, tibble(x = 2:3))
})
