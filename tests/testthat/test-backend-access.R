test_that("custom scalar translated correctly", {
  local_con(simulate_access())

  # Conversion
  expect_equal(test_translate_sql(as.numeric(x)),   sql("CDBL(`x`)"))
  expect_equal(test_translate_sql(as.double(x)),    sql("CDBL(`x`)"))
  expect_equal(test_translate_sql(as.integer(x)),   sql("INT(`x`)"))
  expect_equal(test_translate_sql(as.logical(x)),   sql("CBOOL(`x`)"))
  expect_equal(test_translate_sql(as.character(x)), sql("CSTR(`x`)"))
  expect_equal(test_translate_sql(as.Date(x)),      sql("CDATE(`x`)"))
  # Math
  expect_equal(test_translate_sql(exp(x)),          sql("EXP(`x`)"))
  expect_equal(test_translate_sql(log(x)),          sql("LOG(`x`)"))
  expect_equal(test_translate_sql(log10(x)),        sql("LOG(`x`) / LOG(10)"))
  expect_equal(test_translate_sql(sqrt(x)),         sql("SQR(`x`)"))
  expect_equal(test_translate_sql(sign(x)),         sql("SGN(`x`)"))
  expect_equal(test_translate_sql(floor(x)),        sql("INT(`x`)"))
  expect_equal(test_translate_sql(ceiling(x)),      sql("INT(`x` + 0.9999999999)"))
  expect_equal(test_translate_sql(ceil(x)),         sql("INT(`x` + 0.9999999999)"))
  expect_equal(test_translate_sql(x^y),             sql("`x` ^ `y`"))
  # String
  expect_equal(test_translate_sql(nchar(x)),        sql("LEN(`x`)"))
  expect_equal(test_translate_sql(tolower(x)),      sql("LCASE(`x`)"))
  expect_equal(test_translate_sql(toupper(x)),      sql("UCASE(`x`)"))
  expect_equal(test_translate_sql(substr(x, 1, 2)), sql("RIGHT(LEFT(`x`, 2.0), 2.0)"))
  expect_equal(test_translate_sql(paste(x)),        sql("CSTR(`x`)"))
  expect_equal(test_translate_sql(trimws(x)),       sql("TRIM(`x`)"))
  expect_equal(test_translate_sql(is.null(x)),      sql("ISNULL(`x`)"))
  expect_equal(test_translate_sql(is.na(x)),        sql("ISNULL(`x`)"))
  expect_equal(test_translate_sql(coalesce(x, y)),  sql("IIF(ISNULL(`x`), `y`, `x`)"))
  expect_equal(test_translate_sql(pmin(x, y)),      sql("IIF(`x` <= `y`, `x`, `y`)"))
  expect_equal(test_translate_sql(pmax(x, y)),      sql("IIF(`x` <= `y`, `y`, `x`)"))
  expect_equal(test_translate_sql(Sys.Date()),      sql("DATE()"))
  # paste()
  expect_equal(test_translate_sql(paste(x, y, sep = "+")), sql("`x` & '+' & `y`"))
  expect_equal(test_translate_sql(paste0(x, y)), sql("`x` & `y`"))
  expect_error(test_translate_sql(paste(x, collapse = "-")),"`collapse` not supported")
  # Logic
  expect_equal(test_translate_sql(ifelse(x, "true", "false")), sql("IIF(`x`, 'true', 'false')"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_access())

  expect_equal(test_translate_sql(sd(x, na.rm = TRUE), window = FALSE),  sql("STDEV(`x`)"))
  expect_equal(test_translate_sql(var(x, na.rm = TRUE), window = FALSE), sql("VAR(`x`)"))

  expect_error(test_translate_sql(cor(x), window = FALSE), "not available")
  expect_error(test_translate_sql(cov(x), window = FALSE), "not available")
  expect_error(test_translate_sql(n_distinct(x), window = FALSE), "not available")
})

test_that("custom escaping works as expected", {
  con <- simulate_access()

  expect_equal(escape(TRUE, con = con), sql("-1"))
  expect_equal(escape(FALSE, con = con), sql("0"))
  expect_equal(escape(NA, con = con), sql("NULL"))

  expect_equal(escape(as.Date("2020-01-01"), con = con), sql("#2020-01-01#"))
  expect_equal(escape(as.Date(NA), con = con), sql("NULL"))

  expect_equal(escape(as.POSIXct("2020-01-01", tz = "UTC"), con = con), sql("#2020-01-01 00:00:00#"))
  expect_equal(escape(as.POSIXct(NA, tz = "UTC"), con = con), sql("NULL"))
})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, con = simulate_access())
  expect_snapshot(mf %>% head())
})
