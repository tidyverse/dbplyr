context("translate-teradata")

# Teradata base_scalar conversions -----------------------------------------

test_that("as.numeric() translated to NUMERIC ", {
  expect_equivalent(
    translate_sql(as.numeric(field_name), con = simulate_teradata()),
    sql("CAST(`field_name` AS NUMERIC)")
  )
})

test_that("as.double() translated to NUMERIC ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = simulate_teradata()),
    sql("CAST(`field_name` AS NUMERIC)")
  )
})

test_that("as.character() translate to VARCHAR(MAX) ", {
  expect_equivalent(
  translate_sql(as.character(field_name), con = simulate_teradata()),
    sql("CAST(`field_name` AS VARCHAR(MAX))")
  )
})

test_that("log10() translates to LOG ", {
  expect_equivalent(
    translate_sql(log10(field_name), con = simulate_teradata()),
    sql("LOG(`field_name`)")
  )
})

test_that("ceiling() translates to CEILING ", {
  expect_equivalent(
    translate_sql(ceiling(field_name), con = simulate_teradata()),
    sql("CEILING(`field_name`)")
  )
})

test_that("paste() returns error message", {
  expect_error(
    translate_sql(paste(field_name),
                  window = FALSE,
                  con = simulate_teradata()),
    "PASTE\\(\\) is not supported in this SQL variant, try PASTE0\\(\\) instead"
  )
})

# Teradata base_agg conversions -----------------------------------------

test_that("var() translates to VAR ", {
  expect_equivalent(
    translate_sql(var(field_name),
                  window = FALSE,
                  con = simulate_teradata()),
    sql("VAR_SAMP(`field_name`)")
  )
})

test_that("cor() returns error message", {
  expect_error(
    translate_sql(cor(field_name),
                  window = FALSE,
                  con = simulate_teradata()),
    "COR\\(\\) is not available in this SQL variant"
  )
})

test_that("cov() returns error message", {
  expect_error(
    translate_sql(cov(field_name),
                  window = FALSE,
                  con = simulate_teradata()),
    "COV\\(\\) is not available in this SQL variant"
  )
})

#nchar, atan2, substr, ceil, is.null, is.na, trimws

test_that("nchar() translates to LEN ", {
  expect_equivalent(
    translate_sql(nchar(field_name),
                  con = simulate_teradata()),
    sql("CHARACTER_LENGTH(`field_name`)")
  )
})
test_that("atan2() translates to ATN2 ", {
  expect_equivalent(
    translate_sql(atan2(field1, field2),
                  con = simulate_teradata()),
    sql("ATAN2(`field2`,`field1`)")
  )
})

test_that("substr() translates to SUBSTRING ", {
  expect_equivalent(
    translate_sql(substr(field_name, 1, 2),
                  con = simulate_teradata()),
    sql("SUBSTR(`field_name`, 1.0, 2.0)")
  )
})

test_that("ceil() translates to CEILING ", {
  expect_equivalent(
    translate_sql(ceil(field_name),
                  con = simulate_teradata()),
    sql("CEILING(`field_name`)")
  )
})
test_that("trimws() translates to LTRIM-RTRIM ", {
  expect_equivalent(
    translate_sql(trimws(field_name),
                  con = simulate_teradata()),
    sql("TRIM(`field_name`)")
  )
})


# Teradata query tests  ------------------------------------------------

df <- data.frame(x = 1, y = 2)
df_teradata <- tbl_lazy(df, src = simulate_teradata())
test_that("query uses TOP instead of LIMIT ", {
  expect_equivalent(
    show_query(head(df_teradata)),
    sql("SELECT  TOP 6 *\nFROM `df`"))
})
