context("translate-ACCESS")

# Access base_scalar conversions -----------------------------------------

# Math

test_that("Math special cases", {

  # log10()
  expect_equivalent(
    translate_sql(log10(field_name), con = simulate_odbc_access()),
    sql("LOG(`field_name`) / LOG(10)")
  )

  # floor() and ceiling() / ceil()
  expect_equivalent(
    translate_sql(floor(field_name), con = simulate_odbc_access()),
    sql("INT(`field_name`)")
  )
  expect_equivalent(
    translate_sql(ceiling(field_name), con = simulate_odbc_access()),
    sql("INT(`field_name` + .9999999999)")
  )
  expect_equivalent(
    translate_sql(ceil(field_name), con = simulate_odbc_access()),
    sql("INT(`field_name` + .9999999999)")
  )

  # POW = ^ in Access
  expect_equivalent(
    translate_sql(field_name ^ 2, con = simulate_odbc_access()),
    sql("`field_name` ^ 2.0")
  )

})

# Strings

test_that("String special cases", {

  expect_equivalent(
    translate_sql(substr(field_name, 1, 2), con = simulate_odbc_access()),
    sql("RIGHT(LEFT(`field_name`, 2.0), 2.0)")
  )

})

# Logic

test_that("Logic special cases", {

  # is.null() and is.na() return the same values
  expect_equivalent(
    translate_sql(is.null(field_name), con = simulate_odbc_access()),
    sql("IIF(ISNULL(`field_name`), 1, 0)")
  )
  expect_equivalent(
    translate_sql(is.na(field_name), con = simulate_odbc_access()),
    sql("IIF(ISNULL(`field_name`), 1, 0)")
  )

  # ifelse = IIF in Access
  expect_equivalent(
    translate_sql(ifelse(field_name, yes, no), con = simulate_odbc_access()),
    sql("IIF(`field_name`, `yes`, `no`)")
  )

  # coalesce() for only 2 objects
  expect_equivalent(
    translate_sql(coalesce(field_name_x, field_name_y), con = simulate_odbc_access()),
    sql("IIF(ISNULL(`field_name_x`),`field_name_y`,`field_name_x`)")
  )

  # pmin() and pmax() for only 2 objects
  expect_equivalent(
    translate_sql(pmin(field_name_x, field_name_y), con = simulate_odbc_access()),
    sql("IIF(`field_name_x` <= `field_name_y`,`field_name_x`,`field_name_y`)")
  )
  expect_equivalent(
    translate_sql(pmax(field_name_x, field_name_y), con = simulate_odbc_access()),
    sql("IIF(`field_name_x` <= `field_name_y`,`field_name_y`,`field_name_x`)")
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
