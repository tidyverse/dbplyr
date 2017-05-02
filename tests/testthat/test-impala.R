context("Impala translation")

# Initialize simulated DB connection -------------------------------------

impala_connection <- dbplyr:::simulate_impala()

# Impala base_scalar conversions -----------------------------------------

test_that("as.numeric field coercion translates to Impala's DOUBLE ", {
  expect_equivalent(
    translate_sql(as.numeric(field_name),
                  con = impala_connection),
    sql("CAST(`field_name` AS DOUBLE)")
    )
})
test_that("as.double field coercion translates to Impala's DOUBLE ", {
  expect_equivalent(
    translate_sql(
      as.double(field_name),
      con = impala_connection),
    sql("CAST(`field_name` AS DOUBLE)"))
})
test_that("as.integer field coercion translates to Impala's INT ", {
  expect_equivalent(
    translate_sql(
      as.integer(field_name),
      con = impala_connection),
    sql("CAST(`field_name` AS INT)"))
})
test_that("as.logical field coercion translates to Impala's BOOLEAN ", {
  expect_equivalent(
    translate_sql(
      as.logical(field_name),
      con = impala_connection),
    sql("CAST(`field_name` AS BOOLEAN)"))
})
test_that("as.character field coercion translates to Impala's STRING ", {
  expect_equivalent(
    translate_sql(
      as.character(field_name),
      con = impala_connection),
    sql("CAST(`field_name` AS STRING)"))
})
test_that("as.Date field coercion translates to Impala's VARCHAR(10) ", {
  expect_equivalent(
    translate_sql(
      as.Date(field_name),
      con = impala_connection),
    sql("CAST(`field_name` AS VARCHAR(10))"))
})
test_that("round function translates to Impala's ROUND ", {
  expect_equivalent(
    translate_sql(
      round(10.1),
      con = impala_connection),
    sql("ROUND(10.1, 0)"))
})
test_that("the digits argument of round is passed to Impala's ROUND ", {
  expect_equivalent(
    translate_sql(
      round(10.1, 1),
      con = impala_connection),
    sql("ROUND(10.1, 1)"))
})
test_that("explicit digits argument of round is passed to Impala's ROUND ", {
  expect_equivalent(
    translate_sql(
      round(10.1, digits = 1),
      con = impala_connection),
    sql("ROUND(10.1, 1)"))
})

test_that("| operator translates to Impala's OR ", {
  expect_equivalent(
    translate_sql(
      field_name > 0 |`field_name`< 10,
      con = impala_connection),
    sql("`field_name` > 0.0 OR `field_name` < 10.0"))
})
test_that("|| operator translates to Impala's OR ", {
  expect_equivalent(
    translate_sql(
      field_name > 0 ||`field_name`< 10,
      con = impala_connection),
    sql("`field_name` > 0.0 OR `field_name` < 10.0"))
})
test_that("& operator translates to Impala's AND ", {
  expect_equivalent(
    translate_sql(
      field_name > 0 &`field_name`< 10,
      con = impala_connection),
    sql("`field_name` > 0.0 AND `field_name` < 10.0"))
})
test_that("&& operator translates to Impala's AND ", {
  expect_equivalent(
    translate_sql(
      field_name > 0 &&`field_name`< 10,
      con = impala_connection),
    sql("`field_name` > 0.0 AND `field_name` < 10.0"))
})

# Impala base_agg conversions -----------------------------------------

test_that("count aggregate functions translates to Impala's COUNT(*) ", {
  expect_equivalent(
    translate_sql(count(),
                  window = FALSE,
                  con = impala_connection),
    sql("COUNT(*)"))
})

test_that("n aggregate functions translates to Impala's COUNT(*) ", {
  expect_equivalent(
    translate_sql(n(),
                  window = FALSE,
                  con = impala_connection),
    sql("COUNT(*)"))
})

test_that("n_distinct aggregate functions translates to Impala's COUNT(DISTINCT) ", {
  expect_equivalent(
    translate_sql(n_distinct(field_name),
                  window = FALSE,
                  con = impala_connection),
    sql("COUNT(DISTINCT `field_name`)"))
})

test_that("sd aggregate functions translates to Impala's STDEV ", {
  expect_equivalent(
    translate_sql(sd(field_name),
                  window = FALSE,
                  con = impala_connection),
    sql("SD(`field_name`)"))
})

test_that("var aggregate functions translates to Impala's VARIANCE ", {
  expect_equivalent(
    translate_sql(var(field_name),
                  window = FALSE,
                  con = impala_connection),
    sql("VARIANCE(`field_name`)"))
})

# Impala query tests  ------------------------------------------------

df <- data.frame(x = 1, y = 2)
df_impala <- tbl_lazy(df, src = simulate_impala())

test_that("query uses COUNT(*) for tally instead of COUNT() ", {
  expect_equivalent(
    show_query(tally(df_impala)),
    sql("SELECT COUNT(*) AS `n`\nFROM `df`"))
})

test_that("query uses COUNT(*) for n() instead of COUNT() ", {
  expect_equivalent(
    show_query(summarise(df_impala, count = n())),
    sql("SELECT COUNT(*) AS `count`\nFROM `df`"))
})

test_that("query uses COUNT(*) for tally instead of COUNT() ", {
  expect_equivalent(
    show_query(tally(group_by(df_impala, x))),
    sql("SELECT `x`, COUNT(*) AS `n`\nFROM `df`\nGROUP BY `x`"))
})





