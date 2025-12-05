test_that("custom scalar translated correctly", {
  con <- simulate_access()

  # Conversion
  expect_translation(con, as.numeric(x), "CDBL(`x`)")
  expect_translation(con, as.double(x), "CDBL(`x`)")
  expect_translation(con, as.integer(x), "INT(`x`)")
  expect_translation(con, as.logical(x), "CBOOL(`x`)")
  expect_translation(con, as.character(x), "CSTR(`x`)")
  expect_translation(con, as.Date(x), "CDATE(`x`)")
  # Math
  expect_translation(con, exp(x), "EXP(`x`)")
  expect_translation(con, log(x), "LOG(`x`)")
  expect_translation(con, log10(x), "LOG(`x`) / LOG(10)")
  expect_translation(con, sqrt(x), "SQR(`x`)")
  expect_translation(con, sign(x), "SGN(`x`)")
  expect_translation(con, floor(x), "INT(`x`)")
  expect_translation(con, ceiling(x), "INT(`x` + 0.9999999999)")
  expect_translation(con, ceil(x), "INT(`x` + 0.9999999999)")
  expect_translation(con, x^y, "`x` ^ `y`")
  # String
  expect_translation(con, nchar(x), "LEN(`x`)")
  expect_translation(con, tolower(x), "LCASE(`x`)")
  expect_translation(con, toupper(x), "UCASE(`x`)")
  expect_translation(
    con,
    substr(x, 1, 2),
    "RIGHT(LEFT(`x`, 2.0), 2.0)"
  )
  expect_translation(con, paste(x), "CSTR(`x`)")
  expect_translation(con, trimws(x), "TRIM(`x`)")
  expect_translation(con, is.null(x), "ISNULL(`x`)")
  expect_translation(con, is.na(x), "ISNULL(`x`)")
  expect_translation(
    con,
    coalesce(x, y),
    "IIF(ISNULL(`x`), `y`, `x`)"
  )
  expect_translation(con, pmin(x, y), "IIF(`x` <= `y`, `x`, `y`)")
  expect_translation(con, pmax(x, y), "IIF(`x` <= `y`, `y`, `x`)")
  expect_translation(con, Sys.Date(), "DATE()")
  # paste()
  expect_translation(
    con,
    paste(x, y, sep = "+"),
    "`x` & '+' & `y`"
  )
  expect_translation(con, paste0(x, y), "`x` & `y`")
  expect_snapshot(
    error = TRUE,
    translate_sql(paste(x, collapse = "-"), con = con)
  )
  # Logic
  expect_translation(
    con,
    ifelse(x, "true", "false"),
    "IIF(`x`, 'true', 'false')"
  )
})

test_that("custom aggregators translated correctly", {
  con <- simulate_access()

  expect_translation(
    con,
    sd(x, na.rm = TRUE),
    "STDEV(`x`)",
    window = FALSE
  )
  expect_translation(
    con,
    var(x, na.rm = TRUE),
    "VAR(`x`)",
    window = FALSE
  )

  expect_error(
    translate_sql(cor(x), window = FALSE, con = con),
    class = "dbplyr_error_unsupported_fn"
  )
  expect_error(
    translate_sql(cov(x), window = FALSE, con = con),
    class = "dbplyr_error_unsupported_fn"
  )
  expect_error(
    translate_sql(n_distinct(x), window = FALSE, con = con),
    class = "dbplyr_error_unsupported_fn"
  )
})

test_that("custom escaping works as expected", {
  con <- simulate_access()

  expect_equal(escape(TRUE, con = con), sql("-1"))
  expect_equal(escape(FALSE, con = con), sql("0"))
  expect_equal(escape(NA, con = con), sql("NULL"))

  expect_equal(escape(as.Date("2020-01-01"), con = con), sql("#2020-01-01#"))
  expect_equal(escape(as.Date(NA), con = con), sql("NULL"))

  expect_equal(
    escape(as.POSIXct("2020-01-01", tz = "UTC"), con = con),
    sql("#2020-01-01 00:00:00#")
  )
  expect_equal(escape(as.POSIXct(NA, tz = "UTC"), con = con), sql("NULL"))
})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, con = simulate_access())
  expect_snapshot(mf |> head())
})
