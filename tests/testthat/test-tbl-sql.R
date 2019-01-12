context("test-tbl_sql.R")

test_that("tbl_sql() works with string argument", {
  name <- unclass(random_table_name())
  df <- memdb_frame(a = 1, .name = name)

  expect_equal(collect(tbl_sql("sqlite", df$src, name)), collect(df))
})

test_that("head/print respects n" ,{
  df2 <- memdb_frame(x = 1:5)

  out <- df2 %>% head(n = Inf) %>% collect()
  expect_equal(nrow(out), 5)
  expect_output(print(df2, n = Inf))

  out <- df2 %>% head(n = 1) %>% collect()
  expect_equal(nrow(out), 1)

  out <- df2 %>% head(n = 0) %>% collect()
  expect_equal(nrow(out), 0)

  expect_error(
    df2 %>% head(n = -1) %>% collect(),
    "not greater than or equal to 0"
  )
})

test_that("same_src distinguishes srcs", {
  src1 <- src_sqlite(":memory:", create = TRUE)
  src2 <- src_sqlite(":memory:", create = TRUE)
  expect_true(same_src(src1, src1))
  expect_false(same_src(src1, src2))

  db1 <- copy_to(src1, iris, 'data1', temporary = FALSE)
  db2 <- copy_to(src2, iris, 'data2', temporary = FALSE)
  expect_true(same_src(db1, db1))
  expect_false(same_src(db1, db2))

  expect_false(same_src(db1, mtcars))
})

# tbl ---------------------------------------------------------------------

test_that("can generate sql tbls with raw sql", {
  mf1 <- memdb_frame(x = 1:3, y = 3:1)
  mf2 <- tbl(mf1$src, build_sql("SELECT * FROM ", mf1$ops$x, con = simulate_dbi()))

  expect_equal(collect(mf1), collect(mf2))
})

sqlite_con_with_aux <- function() {
  tmp <- tempfile()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))

  con
}

test_that("can refer to default schema explicitly", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t1 (x)")

  expect_equal(tbl_vars(tbl(con, "t1")), "x")
  expect_equal(tbl_vars(tbl(con, in_schema("main", "t1"))), "x")
})

test_that("can distinguish 'schema.table' from 'schema'.'table'", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE aux.t1 (x, y, z)")
  DBI::dbExecute(con, "CREATE TABLE 'aux.t1' (a, b, c)")

  expect_equal(tbl_vars(tbl(con, in_schema("aux", "t1"))), c("x", "y", "z"))
  expect_equal(tbl_vars(tbl(con, ident("aux.t1"))), c("a", "b", "c"))
})

# copy_to -----------------------------------------------------------------

test_that("can copy to from remote sources", {
  df <- data.frame(x = 1:10)
  con1 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con1), add = TRUE)
  df_1 <- copy_to(con1, df, "df1")

  # Create from tbl in same database
  df_2 <- copy_to(con1, df_1, "df2")
  expect_equal(collect(df_2), df)

  # Create from tbl in another data
  con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  df_3 <- copy_to(con2, df_1, "df3")
  expect_equal(collect(df_3), df)
})

test_that("can round trip basic data frame", {
  df <- test_frame(x = c(1, 10, 9, NA), y = letters[1:4])
  expect_equal_tbls(df)
})

test_that("NAs in character fields handled by db sources (#2256)", {
  df <- test_frame(
    x = c("a", "aa", NA),
    y = c(NA, "b", "bb"),
    z = c("cc", NA, "c")
  )
  expect_equal_tbls(df)
})

test_that("only overwrite existing table if explicitly requested", {
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "df", data.frame(x = 1:5))

  expect_error(copy_to(con, data.frame(x = 1), name = "df"), "exists")
  expect_silent(copy_to(con, data.frame(x = 1), name = "df", overwrite = TRUE))
})

test_that("can create a new table in non-default schema", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  aux_mtcars <- copy_to(con, mtcars, in_schema("aux", "mtcars"), temporary = FALSE)

  expect_equal(tbl_vars(aux_mtcars), tbl_vars(mtcars))
})

# collect/compute/collapse ------------------------------------------------

test_that("collect equivalent to as.data.frame/as_tibble", {
  mf <- memdb_frame(letters)

  expect_equal(as.data.frame(mf), data.frame(letters, stringsAsFactors = FALSE))
  expect_equal(tibble::as_tibble(mf), tibble::tibble(letters))
  expect_equal(collect(mf), tibble::tibble(letters))
})

test_that("explicit collection returns all data", {
  n <- 1e5 + 10 # previous default was 1e5
  big <- memdb_frame(x = seq_len(n))

  nrow1 <- big %>% as.data.frame() %>% nrow()
  nrow2 <- big %>% tibble::as_tibble() %>% nrow()
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


# n_groups ----------------------------------------------------------------

# Data for the first three test_that groups below
df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
# MariaDB returns bit64 instead of int, which makes testing hard
tbls <- test_load(df, ignore = "MariaDB")

test_that("ungrouped data has 1 group, with group size = nrow()", {
  for (tbl in tbls) {
    expect_equal(n_groups(tbl), 1L)
    expect_equal(group_size(tbl), 30)
  }
})

test_that("rowwise data has one group for each group", {
  rw <- rowwise(df)
  expect_equal(n_groups(rw), 30)
  expect_equal(group_size(rw), rep(1, 30))
})

test_that("group_size correct for grouped data", {
  for (tbl in tbls) {
    grp <- group_by(tbl, x)
    expect_equal(n_groups(grp), 3L)
    expect_equal(group_size(grp), rep(10, 3))
  }
})

# tbl_sum -------------------------------------------------------------------

test_that("ungrouped output", {
  mf <- memdb_frame(x = 1:5, y = 1:5, .name = "tbl_sum_test")

  out1 <- tbl_sum(mf)
  expect_named(out1, c("Source", "Database"))
  expect_equal(out1[["Source"]], "table<tbl_sum_test> [?? x 2]")
  expect_match(out1[["Database"]], "sqlite (.*) \\[:memory:\\]")

  out2 <- tbl_sum(mf %>% group_by(x, y))
  expect_named(out2, c("Source", "Database", "Groups"))
  expect_equal(out2[["Groups"]], c("x, y"))

  out3 <- tbl_sum(mf %>% arrange(x))
  expect_named(out3, c("Source", "Database", "Ordered by"))
  expect_equal(out3[["Ordered by"]], c("x"))
})


# pull --------------------------------------------------------------------

test_that("can extract default, by name, or positive/negative position", {
  x <- 1:10
  y <- runif(10)
  mf <- memdb_frame(x = x, y = y)

  expect_equal(pull(mf), y)
  expect_equal(pull(mf, x), x)
  expect_equal(pull(mf, 1L), x)
  expect_equal(pull(mf, -1), y)
})

test_that("extracts correct column from grouped tbl", {
  mf <- memdb_frame(id = "a", value = 42)
  gf <- mf %>% group_by(id)

  expect_equal(pull(mf, value), 42)
})
