context("translate-ACCESS")

# Access base_scalar conversions -----------------------------------------

# Conversion

test_that("as.numeric() translated to CDbl ", {
  expect_equivalent(
    translate_sql(as.numeric(field_name), con = simulate_odbc_access()),
    sql("CDBL(`field_name`)")
  )
})

test_that("as.double() translated to CDbl ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = simulate_odbc_access()),
    sql("CDBL(`field_name`)")
  )
})

test_that("as.integer() translated to Int ", {
  expect_equivalent(
    translate_sql(as.integer(field_name), con = simulate_odbc_access()),
    sql("INT(`field_name`)")
  )
})

test_that("as.logical() translated to CBool ", {
  expect_equivalent(
    translate_sql(as.logical(field_name), con = simulate_odbc_access()),
    sql("CBOOL(`field_name`)")
  )
})

test_that("as.character() translated to CStr ", {
  expect_equivalent(
    translate_sql(as.character(field_name), con = simulate_odbc_access()),
    sql("CSTR(`field_name`)")
  )
})

test_that("as.Date() translated to CDate ", {
  expect_equivalent(
    translate_sql(as.Date(field_name), con = simulate_odbc_access()),
    sql("CDATE(`field_name`)")
  )
})

# Math

test_that("exp() translates to EXP ", {
  expect_equivalent(
    translate_sql(exp(field_name), con = simulate_odbc_access()),
    sql("EXP(`field_name`)")
  )
})

test_that("log() translates to LOG ", {
  expect_equivalent(
    translate_sql(log(field_name), con = simulate_odbc_access()),
    sql("LOG(`field_name`)")
  )
})

test_that("log10() translates to LOG(x) / LOG(10) ", {
  expect_equivalent(
    translate_sql(log10(field_name), con = simulate_odbc_access()),
    sql("LOG(`field_name`) / LOG(10)")
  )
})

test_that("sqrt() translates to SQR ", {
  expect_equivalent(
    translate_sql(sqrt(field_name), con = simulate_odbc_access()),
    sql("SQR(`field_name`)")
  )
})

test_that("sign() translates to SGN ", {
  expect_equivalent(
    translate_sql(sign(field_name), con = simulate_odbc_access()),
    sql("SGN(`field_name`)")
  )
})

test_that("floor() translates to INT ", {
  expect_equivalent(
    translate_sql(floor(field_name), con = simulate_odbc_access()),
    sql("INT(`field_name`)")
  )
})

test_that("ceiling() translates to INT plus nearly 1 ", {
  expect_equivalent(
    translate_sql(ceiling(field_name), con = simulate_odbc_access()),
    sql("INT(`field_name` + .9999999999)")
  )
})

test_that("ceil() translates to INT plus nearly 1 ", {
  expect_equivalent(
    translate_sql(ceil(field_name), con = simulate_odbc_access()),
    sql("INT(`field_name` + .9999999999)")
  )
})

test_that("^ translates to ^ ", {
  expect_equivalent(
    translate_sql(field_name ^ 2, con = simulate_odbc_access()),
    sql("`field_name` ^ 2.0")
  )
})


# Strings

test_that("nchar() translates to LEN ", {
  expect_equivalent(
    translate_sql(nchar(field_name), con = simulate_odbc_access()),
    sql("LEN(`field_name`)")
  )
})

test_that("tolower() translates to LCASE ", {
  expect_equivalent(
    translate_sql(tolower(field_name), con = simulate_odbc_access()),
    sql("LCASE(`field_name`)")
  )
})

test_that("toupper() translates to UCASE ", {
  expect_equivalent(
    translate_sql(toupper(field_name), con = simulate_odbc_access()),
    sql("UCASE(`field_name`)")
  )
})

test_that("substr() translates to RIGHT and LEFT ", {
  expect_equivalent(
    translate_sql(substr(field_name, 1, 2), con = simulate_odbc_access()),
    sql("RIGHT(LEFT(`field_name`, 2.0), 2.0)")
  )
})

test_that("trimws() translates to TRIM ", {
  expect_equivalent(
    translate_sql(trim(field_name), con = simulate_odbc_access()),
    sql("TRIM(`field_name`)")
  )
})

test_that("paste() returns error message", {
  expect_error(
    translate_sql(paste(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    "PASTE\\(\\) is not available in this SQL variant"
  )
})

test_that("paste0() returns error message", {
  expect_error(
    translate_sql(paste0(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    "PASTE0\\(\\) is not available in this SQL variant"
  )
})

# Logic

test_that("is.null() translates to IIF with ISNULL ", {
  expect_equivalent(
    translate_sql(is.null(field_name), con = simulate_odbc_access()),
    sql("IIF(ISNULL(`field_name`), 1, 0)")
  )
})

test_that("is.na() translates to IIF with ISNULL ", {
  expect_equivalent(
    translate_sql(is.na(field_name), con = simulate_odbc_access()),
    sql("IIF(ISNULL(`field_name`), 1, 0)")
  )
})

test_that("ifelse() translates to IIF ", {
  expect_equivalent(
    translate_sql(ifelse(field_name, yes, no), con = simulate_odbc_access()),
    sql("IIF(`field_name`, `yes`, `no`)")
  )
})

test_that("coalesce() translates to IIF with ISNULL for two objects ", {
  expect_equivalent(
    translate_sql(coalesce(field_name_x, field_name_y), con = simulate_odbc_access()),
    sql("IIF(ISNULL(`field_name_x`),`field_name_y`,`field_name_x`)")
  )
})

test_that("pmin() translates to IIF for two objects ", {
  expect_equivalent(
    translate_sql(pmin(field_name_x, field_name_y), con = simulate_odbc_access()),
    sql("IIF(`field_name_x` <= `field_name_y`,`field_name_x`,`field_name_y`)")
  )
})

test_that("pmax() translates to IIF for two objects ", {
  expect_equivalent(
    translate_sql(pmax(field_name_x, field_name_y), con = simulate_odbc_access()),
    sql("IIF(`field_name_x` <= `field_name_y`,`field_name_y`,`field_name_x`)")
  )
})

# Dates

test_that("Sys.Date() translates to DATE ", {
  expect_equivalent(
    translate_sql(Sys.Date(), con = simulate_odbc_access()),
    sql("DATE()")
  )
})

# Access base_agg conversions -----------------------------------------

test_that("mean() translates to AVG ", {
  expect_equivalent(
    translate_sql(mean(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    sql("AVG(`field_name`)")
  )
})

test_that("sd() translates to STDEV ", {
  expect_equivalent(
    translate_sql(sd(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    sql("STDEV(`field_name`)")
  )
})

test_that("var() translates to VAR ", {
  expect_equivalent(
    translate_sql(var(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    sql("VAR(`field_name`)")
  )
})

test_that("max() translates to MAX ", {
  expect_equivalent(
    translate_sql(max(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    sql("MAX(`field_name`)")
  )
})

test_that("min() translates to MIN ", {
  expect_equivalent(
    translate_sql(min(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    sql("MIN(`field_name`)")
  )
})

test_that("cor() returns error message", {
  expect_error(
    translate_sql(cor(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    "COR\\(\\) is not available in this SQL variant"
  )
})

test_that("cov() returns error message", {
  expect_error(
    translate_sql(cov(field_name),
                  window = FALSE,
                  con = simulate_odbc_access()),
    "COV\\(\\) is not available in this SQL variant"
  )
})

# Access query tests  ------------------------------------------------

df <- data.frame(x = 1, y = 2)
df_access <- tbl_lazy(df, src = simulate_odbc_access())
test_that("query uses TOP instead of LIMIT ", {
  expect_equivalent(
    show_query(head(df_access)),
    sql("SELECT TOP 6 *\nFROM `df`"))
})
