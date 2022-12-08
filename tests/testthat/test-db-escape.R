test_that("can translate raw to blob spec", {
  con <- simulate_dbi()

  expect_equal(sql_escape_raw(con, NULL), "NULL")
  expect_equal(sql_escape_raw(con, charToRaw("abc")), "X'616263'")
})
