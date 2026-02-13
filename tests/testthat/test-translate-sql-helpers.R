test_that("missing window functions create a warning", {
  con <- dialect_ansi()
  sim_scalar <- sql_translator()
  sim_agg <- sql_translator(`+` = sql_infix("+"))
  sim_win <- sql_translator()

  expect_warning(
    sql_variant(sim_scalar, sim_agg, sim_win),
    "Translator is missing"
  )
})

test_that("duplicates throw an error", {
  expect_snapshot(error = TRUE, {
    sql_translator(round = \(x) x, round = \(y) y)
  })
})

test_that("missing aggregate functions filled in", {
  sim_scalar <- sql_translator()
  sim_agg <- sql_translator()
  sim_win <- sql_translator(mean = \() {})

  trans <- sql_variant(sim_scalar, sim_agg, sim_win)
  expect_error(trans$aggregate$mean(), "only available in a window")
})

test_that("output of print method for sql_variant is correct", {
  sim_trans <- sql_translator(`+` = sql_infix("+"))
  expect_snapshot(sql_variant(sim_trans, sim_trans, sim_trans))
})

test_that("win_rank() is accepted by the sql_translator", {
  expect_snapshot(
    sql_variant(
      sql_translator(
        test = win_rank("test")
      )
    )
  )
})
