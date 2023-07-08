test_that("custom scalar translated correctly", {
  local_con(simulate_teradata())

  expect_equal(test_translate_sql(x != y),          sql("`x` <> `y`"))
  expect_equal(test_translate_sql(as.numeric(x)),   sql("CAST(`x` AS DECIMAL(12, 9))"))
  expect_equal(test_translate_sql(as.numeric(x, 8)),   sql("CAST(`x` AS DECIMAL(12, 8))"))
  expect_equal(test_translate_sql(as.double(x)),    sql("CAST(`x` AS NUMERIC)"))
  expect_equal(test_translate_sql(as.character(x)), sql("CAST(`x` AS VARCHAR(MAX))"))
  expect_equal(test_translate_sql(log(x)),          sql("LN(`x`)"))
  expect_equal(test_translate_sql(cot(x)),          sql("1 / TAN(`x`)"))
  expect_equal(test_translate_sql(nchar(x)),        sql("CHARACTER_LENGTH(`x`)"))
  expect_equal(test_translate_sql(ceil(x)),         sql("CEILING(`x`)"))
  expect_equal(test_translate_sql(ceiling(x)),      sql("CEILING(`x`)"))
  expect_equal(test_translate_sql(atan2(x, y)),     sql("ATAN2(`y`, `x`)"))
  expect_equal(test_translate_sql(substr(x, 1, 2)), sql("SUBSTR(`x`, 1.0, 2.0)"))
  expect_equal(test_translate_sql(startsWith(x,"A")), sql("CAST(CASE WHEN INSTR(`x`, 'A') = 1 THEN 1 ELSE 0 END AS INTEGER)"))
  expect_equal(test_translate_sql(paste0(x,y)),     sql("`x` || `y`"))
  expect_equal(test_translate_sql(paste(x,y)),      sql("`x` || ' ' || `y`"))
  expect_equal(test_translate_sql(as.Date("2020-01-01")), sql("DATE '2020-01-01'"))
  expect_equal(test_translate_sql(as.Date(sql("x + 1"))), sql("CAST(x + 1 AS DATE)"))
  expect_equal(test_translate_sql(as.Date(x)),      sql("CAST(`x` AS DATE)"))
  expect_equal(test_translate_sql(week(as.Date("2020-01-01"))), sql("WEEKNUMBER_OF_YEAR(DATE '2020-01-01', 'iso')"))
  expect_equal(test_translate_sql(quarter(as.Date("2020-01-01"))), sql("to_char(DATE '2020-01-01', 'q')"))

})

test_that("custom bitwise operations translated correctly", {
  local_con(simulate_teradata())

  expect_equal(test_translate_sql(bitwNot(x)),        sql("BITNOT(`x`)"))
  expect_equal(test_translate_sql(bitwAnd(x, 128L)),  sql("BITAND(`x`, 128)"))
  expect_equal(test_translate_sql(bitwOr(x, 128L)),   sql("BITOR(`x`, 128)"))
  expect_equal(test_translate_sql(bitwXor(x, 128L)),  sql("BITXOR(`x`, 128)"))
  expect_equal(test_translate_sql(bitwShiftL(x, 2L)), sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(test_translate_sql(bitwShiftR(x, 2L)), sql("SHIFTRIGHT(`x`, 2)"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_teradata())

  expect_equal(test_translate_sql(var(x, na.rm = TRUE), window = FALSE), sql("VAR_SAMP(`x`)"))
  expect_equal(test_translate_sql(cumsum(x, order_by = c(A,B))),     sql("SUM(`x`) OVER (ORDER BY `A`, `B` ROWS UNBOUNDED PRECEDING)"))
})

test_that("custom window functions translated correctly", {
  local_con(simulate_teradata())

  expect_equal(test_translate_sql(var(x, na.rm = TRUE)), sql("VAR_SAMP(`x`) OVER ()"))
  expect_equal(test_translate_sql(cumsum(x, order_by = c(A,B))),     sql("SUM(`x`) OVER (ORDER BY `A`, `B` ROWS UNBOUNDED PRECEDING)"))
})

test_that("generates custom sql", {
  con <- simulate_teradata()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
})

# verb translation --------------------------------------------------------

test_that("head translated to TOP", {
  mf <- lazy_frame(x = 1, con = simulate_teradata())
  expect_snapshot(mf %>% head() %>% sql_render())
})

test_that("lead, lag work", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = simulate_teradata())

  expect_snapshot(mf %>% group_by(y) %>% mutate(val2 = lead(x, order_by = x)) %>% sql_render())
  expect_snapshot(mf %>% group_by(y) %>% mutate(val2 = lag(x, order_by = x)) %>% sql_render())


})


test_that("weighted.mean", {
  mf <- lazy_frame(x = c(1:5), y = c(6:10), con = simulate_teradata())

  expect_snapshot(mf %>% summarise(wt_mean = weighted.mean(x, y)))


})

test_that("row_number() with and without group_by() and arrange(): unordered defaults to Ordering by NULL (per use_default_order_null)", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = simulate_teradata())
  expect_snapshot(mf %>% mutate(rown = row_number()))
  expect_snapshot(mf %>% group_by(y) %>% mutate(rown = row_number()))
  expect_snapshot(mf %>% arrange(y) %>% mutate(rown = row_number()))
})

test_that("head after distinct() produces subquery", {
  lf <- lazy_frame(x = 1, y = 2, con = simulate_teradata())
  expect_snapshot(lf %>% distinct() %>% head())
})
