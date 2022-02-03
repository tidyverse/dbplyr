# function translation ----------------------------------------------------

test_that("custom scalar translated correctly", {
  local_con(simulate_mysql())
  expect_equal(translate_sql(as.logical(1L)), sql("IF(1, TRUE, FALSE)"))

  expect_equal(translate_sql(str_locate("abc", "b")), sql("REGEXP_INSTR('abc', 'b')"))
  expect_equal(translate_sql(str_replace_all("abc", "b", "d")), sql("REGEXP_REPLACE('abc', 'b', 'd')"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_mysql())

  expect_equal(translate_sql(str_flatten(y, ","), window = FALSE),  sql("GROUP_CONCAT(`y` SEPARATOR ',')"))
})

test_that("use CHAR type for as.character", {
  local_con(simulate_mysql())
  expect_equal(translate_sql(as.character(x)), sql("CAST(`x` AS CHAR)"))
})


# verbs -------------------------------------------------------------------

test_that("generates custom sql", {
  con <- simulate_mysql()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_explain(con, sql("SELECT * FROM table")))

  lf <- lazy_frame(x = 1, con = con)
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))

  expect_snapshot(error = TRUE, full_join(lf, lf, by = "x"))

  expect_snapshot(slice_sample(lf, 5))
})

# live database -----------------------------------------------------------

test_that("logicals converted to integer correctly", {
  db <- copy_to_test("MariaDB", data.frame(x = c(TRUE, FALSE, NA)))
  expect_identical(db %>% pull(), c(1L, 0L, NA))
})

test_that("can explain", {
  db <- copy_to_test("MariaDB", data.frame(x = 1:3))
  expect_snapshot(db %>% mutate(y = x + 1) %>% explain())
})

test_that("can overwrite temp tables", {
  con <- src_test("MariaDB")

  df1 <- tibble(x = 1)
  copy_to(con, df1, "test-df", temporary = TRUE)

  df2 <- tibble(x = 2)
  db2 <- copy_to(con, df2, "test-df", temporary = TRUE, overwrite = TRUE)
  expect_equal(collect(db2), df2)
})
