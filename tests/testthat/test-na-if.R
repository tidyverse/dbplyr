context("na_if")

test_that("is translated to NULL_IF", {
  expect_equal(translate_sql(na_if(x, 0L)), sql('NULL_IF("x", 0)'))
})
