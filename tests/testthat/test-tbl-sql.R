context("tbl_sql")

test_that("can generate sql tbls with raw sql", {
  mf1 <- memdb_frame(x = 1:3, y = 3:1)
  mf2 <- tbl(mf1$src, build_sql("SELECT * FROM ", mf1$ops$x, con = simulate_dbi()))

  expect_equal(collect(mf1), collect(mf2))
})

test_that("tbl_sql() works with string argument", {
  name <- unclass(random_table_name())
  df <- memdb_frame(a = 1, .name = name)

  expect_equal(collect(tbl_sql("sqlite", df$src, name)), collect(df))
})

test_that("memdb_frame() returns visible output", {
  expect_true(withVisible(memdb_frame(a = 1))$visible)
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

test_that("db_write_table calls dbQuoteIdentifier on table name" ,{
  idents <- character()

  setClass("DummyDBIConnection", representation("DBIConnection"))
  setMethod("dbQuoteIdentifier", c("DummyDBIConnection", "character"),
    function(conn, x, ...) {
      idents <<- c(idents, x)
    }
  )

  setMethod("dbWriteTable", c("DummyDBIConnection", "character", "ANY"),
    function(conn, name, value, ...) {TRUE}
  )

  dummy_con <- new("DummyDBIConnection")
  db_write_table(dummy_con, "somecrazytablename", NA, NA)
  expect_true("somecrazytablename" %in% idents)
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

# copy_to -----------------------------------------------------------------

test_that("can copy to from remote sources", {
  df <- data.frame(x = 1:10)
  con1 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con1))
  df_1 <- copy_to(con1, df, "df1")

  # Create from tbl in same database
  df_2 <- copy_to(con1, df_1, "df2")
  expect_equal(collect(df_2), df)

  # Create from tbl in another data
  con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con2))
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

