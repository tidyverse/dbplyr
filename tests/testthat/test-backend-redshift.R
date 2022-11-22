test_that("defaults to postgres translations", {
  local_con(simulate_redshift())
  expect_equal(translate_sql(log10(x)), sql("LOG(`x`)"))
})

test_that("string translations", {
  local_con(simulate_redshift())

  expect_error(translate_sql(str_replace("xx", ".", "a")), "not available")
  expect_equal(translate_sql(str_replace_all("xx", ".", "a")), sql("REGEXP_REPLACE('xx', '.', 'a')"))

  expect_equal(translate_sql(substr(x, 2, 2)), sql("SUBSTRING(`x`, 2, 1)"))
  expect_equal(translate_sql(str_sub(x, 2, -2)), sql("SUBSTRING(`x`, 2, LEN(`x`) - 2)"))

  expect_equal(translate_sql(paste("x", "y")), sql("'x' || ' ' || 'y'"))
  expect_equal(translate_sql(paste0("x", "y")), sql("'x' || 'y'"))
  expect_equal(translate_sql(str_c("x", "y")), sql("'x' || 'y'"))
})

test_that("numeric translations", {
  local_con(simulate_redshift())

  expect_equal(translate_sql(as.numeric(x)), sql("CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(as.double(x)), sql("CAST(`x` AS FLOAT)"))
  expect_equal(translate_sql(round(1.234, 1)), sql("ROUND((1.234) :: float, 1)"))
})

test_that("aggregate functions", {
  local_con(simulate_redshift())

  expect_equal(translate_sql(str_flatten(x, y), window = FALSE), sql("LISTAGG(`x`, `y`)"))
  expect_equal(translate_sql(str_flatten(x, y), window = TRUE), sql("LISTAGG(`x`, `y`) OVER ()"))
  expect_equal(translate_sql(order_by(z, str_flatten(x, y))), sql("LISTAGG(`x`, `y`) WITHIN GROUP (ORDER BY `z`) OVER ()"))
})

test_that("lag and lead translation", {
  local_con(simulate_redshift())

  expect_equal(translate_sql(lead(x)), sql("LEAD(`x`, 1) OVER ()"))
  expect_equal(translate_sql(lag(x)), sql("LAG(`x`, 1) OVER ()"))

  expect_error(translate_sql(lead(x, default = y)), "unused argument")
  expect_error(translate_sql(lag(x, default = y)), "unused argument")
})

test_that("copy_inline uses UNION ALL", {
  con <- simulate_redshift()
  y <- tibble::tibble(id = 1L, arr = "{1,2,3}")

  types <- c(id = "bigint", arr = "integer[]")
  expect_snapshot({
    copy_inline(con, y %>% slice(0)) %>% remote_query()
    copy_inline(con, y) %>% remote_query()

    # with `types`
    copy_inline(con, y %>% slice(0), types = types) %>% remote_query()
    copy_inline(con, y, types = types) %>% remote_query()
  })
})
