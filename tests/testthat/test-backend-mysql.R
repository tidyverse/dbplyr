test_that("use CHAR type for as.character", {
  local_con(simulate_mysql())
  expect_equal(translate_sql(as.character(x)), sql("CAST(`x` AS CHAR)"))
})

test_that("generates custom sql", {
  con <- simulate_mysql()

  expect_snapshot(sql_analyze(con, ident("table")))
  expect_snapshot(sql_explain(con, sql("SELECT * FROM table")))
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
  src <- src_test("MariaDB")
  copy_to(src, mtcars, "mtcars", overwrite = TRUE)
  expect_error(copy_to(src, mtcars, "mtcars", overwrite = TRUE), NA)
})
