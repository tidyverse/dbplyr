test_that("simulate_odbc() still works", {
  expect_translation(simulate_odbc(), x + 1, '"x" + 1.0')
})

test_that("custom scalar translated correctly", {
  con <- dialect_odbc()

  expect_translation(con, as.numeric(x), 'CAST("x" AS DOUBLE)')
  expect_translation(con, as.double(x), 'CAST("x" AS DOUBLE)')
  expect_translation(con, as.integer(x), 'CAST("x" AS INT)')
  expect_translation(con, as.character(x), 'CAST("x" AS STRING)')
})

test_that("custom aggregators translated correctly", {
  con <- dialect_odbc()

  expect_translation(
    con,
    sd(x, na.rm = TRUE),
    'STDDEV_SAMP("x")',
    window = FALSE
  )
})

test_that("custom window functions translated correctly", {
  con <- dialect_odbc()

  expect_translation(
    con,
    sd(x, na.rm = TRUE),
    'STDDEV_SAMP("x") OVER ()'
  )
})
