context("Hive translation")

# Initialize simulated DB connection -------------------------------------

hive_connection <- dbplyr:::simulate_hive()

# Hive base_scalar conversions -----------------------------------------

test_that("as.numeric field coercion translates to Hive's DOUBLE ", {
  expect_equivalent(
    translate_sql(as.numeric(field_name), con = hive_connection),
    sql("CAST( field_name  AS DOUBLE)")
    )
})
test_that("as.double field coercion translates to Hive's DOUBLE ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = hive_connection),
    sql("CAST( field_name  AS DOUBLE)"))
})
test_that("as.integer field coercion translates to Hive's INT ", {
  expect_equivalent(
    translate_sql(as.integer(field_name), con = hive_connection),
    sql("CAST( field_name  AS INT)"))
})
test_that("as.logical field coercion translates to Hive's BOOLEAN ", {
  expect_equivalent(
    translate_sql(as.logical(field_name), con = hive_connection),
    sql("CAST( field_name  AS BOOLEAN)"))
})
test_that("as.character field coercion translates to Hive's STRING ", {
  expect_equivalent(
    translate_sql(as.character(field_name), con = hive_connection),
    sql("CAST( field_name  AS STRING)"))
})
test_that("as.Date field coercion translates to Hive's DATE ", {
  expect_equivalent(
    translate_sql(as.Date(field_name), con = hive_connection),
    sql("CAST( field_name  AS DATE)"))
})
test_that("round function translates to Hive's ROUND ", {
  expect_equivalent(
    translate_sql(round(10.1), con = hive_connection),
    sql("ROUND(10.1, 0)"))
})
test_that("the digits argument of round is passed to Hive's ROUND ", {
  expect_equivalent(
    translate_sql(round(10.1, 1), con = hive_connection),
    sql("ROUND(10.1, 1)"))
})
test_that("explicit digits argument of round is passed to Hive's ROUND ", {
  expect_equivalent(
    translate_sql(round(10.1, digits = 1), con = hive_connection),
    sql("ROUND(10.1, 1)"))
})

test_that("| operator translates to Hive's OR ", {
  expect_equivalent(
    translate_sql(field_name > 0 | field_name < 10, con = hive_connection),
    sql(" field_name  > 0.0 OR  field_name  < 10.0"))
})
test_that("|| operator translates to Hive's OR ", {
  expect_equivalent(
    translate_sql(field_name > 0 || field_name < 10, con = hive_connection),
    sql(" field_name  > 0.0 OR  field_name  < 10.0"))
})
test_that("& operator translates to Hive's AND ", {
  expect_equivalent(
    translate_sql(field_name > 0 & field_name < 10, con = hive_connection),
    sql(" field_name  > 0.0 AND  field_name  < 10.0"))
})
test_that("&& operator translates to Hive's AND ", {
  expect_equivalent(
    translate_sql(field_name > 0 && field_name < 10, con = hive_connection),
    sql(" field_name  > 0.0 AND  field_name  < 10.0"))
})




# Hive base_agg conversions -----------------------------------------

test_that("count aggregate functions translates to Hive's COUNT(*) ", {
  expect_equivalent(
    translate_sql(count(),
                  window = FALSE,
                  con = hive_connection),
    sql("COUNT(*)"))
})

test_that("n aggregate functions translates to Hive's COUNT(*) ", {
  expect_equivalent(
    translate_sql(n(),
                  window = FALSE,
                  con = hive_connection),
    sql("COUNT(*)"))
})

test_that("n_distinct aggregate functions translates to Hive's COUNT(DISTINCT) ", {
  expect_equivalent(
    translate_sql(n_distinct(field_name),
                  window = FALSE,
                  con = hive_connection),
    sql("COUNT(DISTINCT( field_name ))"))
})

test_that("sd aggregate functions translates to Hive's STDEV ", {
  expect_equivalent(
    translate_sql(sd(field_name),
                  window = FALSE,
                  con = hive_connection),
    sql("SD( field_name )"))
})

test_that("var aggregate functions translates to Hive's VARIANCE ", {
  expect_equivalent(
    translate_sql(var(field_name),
                  window = FALSE,
                  con = hive_connection),
    sql("VARIANCE( field_name )"))
})

# Hive query tests  ------------------------------------------------

df <- data.frame(x = 1, y = 2)
df_hive <- tbl_lazy(df, src = simulate_hive())

test_that("query uses COUNT(*) for tally instead of COUNT() ", {
  expect_equivalent(
    show_query(tally(df_hive)),
    sql("SELECT COUNT(*) AS  n \nFROM  df "))
})

test_that("query uses COUNT(*) for n() instead of COUNT() ", {
  expect_equivalent(
    show_query(summarise(df_hive, count = n())),
    sql("SELECT COUNT(*) AS  count \nFROM  df "))
})

test_that("query uses COUNT(*) for tally instead of COUNT() ", {
  expect_equivalent(
    show_query(tally(group_by(df_hive, x))),
    sql("SELECT  x , COUNT(*) AS  n \nFROM  df \nGROUP BY  x "))
})





