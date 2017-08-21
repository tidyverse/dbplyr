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

# Trig

test_that("atan() translates correctly ", {
  expect_equivalent(
    translate_sql(atan(field_name), con = simulate_odbc_access()),
    sql("ATN(`field_name`)")
  )
})

test_that("acos() translates to ATN ", {
  expect_equivalent(
    translate_sql(acos(field_name), con = simulate_odbc_access()),
    sql("IIF(ABS(`field_name`) = 1, `field_name`* -2 * ATN(1) + 2 * ATN(1), ATN(-`field_name`/ SQR(-`field_name`*`field_name` + 1)) + 2 * ATN(1))")
  )
})

test_that("acosh() translates correctly ", {
  expect_equivalent(
    translate_sql(acosh(field_name), con = simulate_odbc_access()),
    sql("LOG(`field_name` + SQR(`field_name`^2 - 1))")
  )
})

test_that("asin() translates correctly ", {
  expect_equivalent(
    translate_sql(asin(field_name), con = simulate_odbc_access()),
    sql("IIF(ABS(`field_name`) = 1, `field_name`* 2 * ATN(1), ATN(`field_name`/ SQR(-`field_name`*`field_name` + 1)))")
  )
})

test_that("asinh() translates correctly ", {
  expect_equivalent(
    translate_sql(asinh(field_name), con = simulate_odbc_access()),
    sql("LOG(`field_name` + SQR(`field_name`^2 + 1))")
  )
})

test_that("atan2() translates correctly ", {
  expect_equivalent(
    translate_sql(atan2(field_name_y, field_name_x), con = simulate_odbc_access()),
    sql("IIF(`field_name_y` > 0, IIF(`field_name_x` >= `field_name_y`, ATN(`field_name_y` / `field_name_x`), IIF(`field_name_x` <= -`field_name_y`, ATN(`field_name_y` / `field_name_x`+ ATN(1) * 4), ATN(1) * 2 - ATN(`field_name_x` / `field_name_y`))), IIF(`field_name_x`>= -`field_name_y`, ATN(`field_name_y`/`field_name_x`), IIF(`field_name_x`<=`field_name_y`, ATN(`field_name_y`/`field_name_x`- ATN(1)*4), -ATN(1)*2 - ATN(`field_name_x`/`field_name_y`))))")
  )
})

test_that("atanh() translates correctly ", {
  expect_equivalent(
    translate_sql(atanh(field_name), con = simulate_odbc_access()),
    sql("LOG((1 + `field_name`) / (1 - `field_name`)) / 2")
  )
})

test_that("cot() translates correctly ", {
  expect_equivalent(
    translate_sql(cot(field_name), con = simulate_odbc_access()),
    sql("1 / TAN(`field_name`)")
  )
})

test_that("coth() translates correctly ", {
  expect_equivalent(
    translate_sql(coth(field_name), con = simulate_odbc_access()),
    sql("(EXP(`field_name`) + EXP(-`field_name`)) / (EXP(`field_name`) - EXP(-`field_name`))")
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

# Access base_win conversions -----------------------------------------

test_that("sd() returns error message", {
  expect_error(
    translate_sql(sd(field_name),
                  window = TRUE,
                  con = simulate_odbc_access()),
    "Window function `sd\\(\\)` is not supported by this database"
  )
})

test_that("n() returns error message", {
  expect_error(
    translate_sql(n(field_name),
                  window = TRUE,
                  con = simulate_odbc_access()),
    "Window function `n\\(\\)` is not supported by this database"
  )
})

test_that("cor() returns error message", {
  expect_error(
    translate_sql(cor(field_name),
                  window = TRUE,
                  con = simulate_odbc_access()),
    "Window function `cor\\(\\)` is not supported by this database"
  )
})

test_that("cov() returns error message", {
  expect_error(
    translate_sql(cov(field_name),
                  window = TRUE,
                  con = simulate_odbc_access()),
    "Window function `cov\\(\\)` is not supported by this database"
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
