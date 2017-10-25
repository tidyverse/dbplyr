context("translate-MySQL")

test_that("use CHAR type for as.character", {
  expect_equivalent(
    translate_sql(as.character(x), con = simulate_mysql()),
    sql("CAST(`x` AS CHAR)")
  )
})

test_that("logicals converted to integer correctly", {
  skip_if_no_db("mysql")

  df1 <- data.frame(x = c(TRUE, FALSE, NA))
  df2 <- src_test("mysql") %>% copy_to(df1, random_table_name()) %>% collect()

  expect_identical(df2$x, c(1L, 0L, NA))
})
