context("translate-MSSQL")

# MSSQL base_scalar conversions -----------------------------------------

test_that("as.numeric() translated to NUMERIC ", {
  expect_equivalent(
    translate_sql(as.numeric(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS NUMERIC)")
  )
})

test_that("as.double() translated to NUMERIC ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS NUMERIC)")
  )
})

test_that("as.character() translate to VARCHAR(MAX) ", {
  expect_equivalent(
  translate_sql(as.character(field_name), con = simulate_mssql()),
    sql("CAST(`field_name` AS VARCHAR(MAX))")
  )
})
test_that("log() translates to LOG ", {
  expect_equivalent(
    translate_sql(log(field_name), con = simulate_mssql()),
    sql("LOG(`field_name`)")
  )
})
test_that("ceiling() translates to CEILING ", {
  expect_equivalent(
    translate_sql(ceiling(field_name), con = simulate_mssql()),
    sql("CEILING(`field_name`)")
  )
})

test_that("paste() returns error message", {
  expect_error(
    translate_sql(paste(field_name),
                  window = FALSE,
                  con = simulate_mssql()),
    "PASTE\\(\\) is not available in this SQL variant"
  )
})

# MSSQL base_agg conversions -----------------------------------------

test_that("sd() translates to STDEV ", {
  expect_equivalent(
    translate_sql(sd(field_name, na.rm = TRUE),
                  window = FALSE,
                  con = simulate_mssql()),
    sql("STDEV(`field_name`)")
  )
})

test_that("var() translates to VAR ", {
  expect_equivalent(
    translate_sql(var(field_name, na.rm = TRUE),
                  window = FALSE,
                  con = simulate_mssql()),
    sql("VAR(`field_name`)")
  )
})

test_that("cor() returns error message", {
  expect_error(
    translate_sql(cor(field_name),
                  window = FALSE,
                  con = simulate_mssql()),
    "COR\\(\\) is not available in this SQL variant"
  )
})

test_that("cov() returns error message", {
  expect_error(
    translate_sql(cov(field_name),
                  window = FALSE,
                  con = simulate_mssql()),
    "COV\\(\\) is not available in this SQL variant"
  )
})

#nchar, atan2, substr, ceil, is.null, is.na, trimws

test_that("nchar() translates to LEN ", {
  expect_equivalent(
    translate_sql(nchar(field_name),
                  con = simulate_mssql()),
    sql("LEN(`field_name`)")
  )
})
test_that("atan2() translates to ATN2 ", {
  expect_equivalent(
    translate_sql(atan2(field_name),
                  con = simulate_mssql()),
    sql("ATN2(`field_name`)")
  )
})
test_that("substr() translates to SUBSTRING ", {
  expect_equivalent(
    translate_sql(substr(field_name, 1, 2),
                  con = simulate_mssql()),
    sql("SUBSTRING(`field_name`, 1.0, 2.0)")
  )
})
test_that("ceil() translates to CEILING ", {
  expect_equivalent(
    translate_sql(ceil(field_name),
                  con = simulate_mssql()),
    sql("CEILING(`field_name`)")
  )
})
test_that("trimws() translates to LTRIM-RTRIM ", {
  expect_equivalent(
    translate_sql(trimws(field_name),
                  con = simulate_mssql()),
    sql("LTRIM(RTRIM(`field_name`))")
  )
})

# stringr -------------------------------------------

test_that("str_length() translates correctly ", {
  expect_equivalent(
    translate_sql(
      str_length(field_name),
      con = simulate_mssql()),
    sql("LEN(`field_name`)"))
})

test_that("str_locate() translates correctly ", {
  expect_equivalent(
    translate_sql(
      str_locate(field_name, "find"),
      con = simulate_mssql()),
    sql("CHARINDEX('find', `field_name`)"))
})

test_that("str_detect() translates correctly ", {
  expect_equivalent(
    translate_sql(
      str_detect(field_name, "find"),
      con = simulate_mssql()),
    sql("CHARINDEX('find', `field_name`) > 0"))
})


# MSSQL query tests  ------------------------------------------------

df <- data.frame(x = 1, y = 2)
df_mssql <- tbl_lazy(df, src = simulate_mssql())

test_that("query uses TOP instead of LIMIT ", {
  expect_equivalent(
    show_query(head(df_mssql)),
    sql("SELECT  TOP 6 *\nFROM `df`"))
})

test_that("Mutate uses CONVERT-BIT for is.na ", {
  expect_equivalent(
    show_query(mutate(df_mssql, z = is.na(x))),
    sql("SELECT `x`, `y`, CONVERT(BIT, IIF(`x` IS NULL, 1, 0)) AS `z`\nFROM `df`"))
})

test_that("Filter uses IS NULL for is.na ", {
  expect_equivalent(
    show_query(filter(df_mssql, is.na(x))),
    sql("SELECT *\nFROM `df`\nWHERE (((`x`) IS NULL))"))
})

test_that("Filter uses NOT IS NULL for !is.na ", {
  expect_equivalent(
    show_query(filter(df_mssql, !is.na(x))),
    sql("SELECT *\nFROM `df`\nWHERE (NOT(((`x`) IS NULL)))"))
})

test_that("Arrange uses CONVERT-BIT for is.na ", {
  expect_equivalent(
    show_query(arrange(df_mssql, !is.na(x))),
    sql("SELECT *\nFROM `df`\nORDER BY NOT(CONVERT(BIT, IIF(`x` IS NULL, 1, 0)))"))
})
