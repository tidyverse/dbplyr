test_that("aggregation functions warn if na.rm = FALSE", {
  local_con(simulate_dbi())
  sql_mean <- sql_aggregate("MEAN")

  expect_warning(sql_mean("x"), "Missing values")
  expect_warning(sql_mean("x", na.rm = TRUE), NA)
})

test_that("missing window functions create a warning", {
  local_con(simulate_dbi())
  sim_scalar <- sql_translator()
  sim_agg <- sql_translator(`+` = sql_infix("+"))
  sim_win <- sql_translator()

  expect_warning(
    sql_variant(sim_scalar, sim_agg, sim_win),
    "Translator is missing"
  )
})

test_that("missing aggregate functions filled in", {
  local_con(simulate_dbi())
  sim_scalar <- sql_translator()
  sim_agg <- sql_translator()
  sim_win <- sql_translator(mean = function() {})

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
