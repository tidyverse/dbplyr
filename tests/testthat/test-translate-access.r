context("translate-ACCESS")


test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_odbc_access())
  }
  # Conversion
  expect_equal(trans(as.numeric(x)),   sql("CDBL(`x`)"))
  expect_equal(trans(as.double(x)),    sql("CDBL(`x`)"))
  expect_equal(trans(as.integer(x)),   sql("INT(`x`)"))
  expect_equal(trans(as.logical(x)),   sql("CBOOL(`x`)"))
  expect_equal(trans(as.character(x)), sql("CSTR(`x`)"))
  expect_equal(trans(as.Date(x)),      sql("CDATE(`x`)"))
  # Math
  expect_equal(trans(exp(x)),          sql("EXP(`x`)"))
  expect_equal(trans(log(x)),          sql("LOG(`x`)"))
  expect_equal(trans(log10(x)),        sql("LOG(`x`) / LOG(10)"))
  expect_equal(trans(sqrt(x)),         sql("SQR(`x`)"))
  expect_equal(trans(sign(x)),         sql("SGN(`x`)"))
  expect_equal(trans(floor(x)),        sql("INT(`x`)"))
  expect_equal(trans(ceiling(x)),      sql("INT(`x` + 0.9999999999)"))
  expect_equal(trans(ceil(x)),         sql("INT(`x` + 0.9999999999)"))
  # String
  expect_equal(trans(nchar(x)),        sql("LEN(`x`)"))
  expect_equal(trans(tolower(x)),      sql("LCASE(`x`)"))
  expect_equal(trans(toupper(x)),      sql("UCASE(`x`)"))
  expect_equal(trans(substr(x, 1, 2)), sql("RIGHT(LEFT(`x`, 2.0), 2.0)"))
  expect_equal(trans(paste(x)),        sql("CSTR(`x`)"))
  expect_equal(trans(trimws(x)),       sql("TRIM(`x`)"))
  expect_equal(trans(is.null(x)),      sql("ISNULL(`x`)"))
  expect_equal(trans(is.na(x)),        sql("ISNULL(`x`)"))
  expect_equal(trans(coalesce(x, y)),  sql("IIF(ISNULL(`x`), `y`, `x`)"))
  expect_equal(trans(pmin(x, y)),      sql("IIF(`x` <= `y`, `x`, `y`)"))
  expect_equal(trans(pmax(x, y)),      sql("IIF(`x` <= `y`, `y`, `x`)"))
  expect_equal(trans(Sys.Date()),      sql("DATE()"))


  # Special paste() tests
  expect_equal(trans(paste(x, y, sep = "+")), sql("`x` & '+' & `y`"))
  expect_equal(trans(paste0(x, y)), sql("`x` & `y`"))

  expect_error(trans(paste(x, collapse = "-")),"`collapse` not supported")

})

test_that("custom aggregators translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = FALSE, con = simulate_odbc_access())
  }

  expect_equal(trans(sd(x)),  sql("STDEV(`x`)"))
  expect_equal(trans(var(x)), sql("VAR(`x`)"))

  expect_error(trans(cor(x)), "not available")
  expect_error(trans(cov(x)), "not available")
  expect_error(trans(n_distinct(x)), "not available")

})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, src = simulate_odbc_access())

  expect_equal(
    mf %>% head() %>% show_query(),
    sql("SELECT TOP 6 *\nFROM `df`")
  )

})
