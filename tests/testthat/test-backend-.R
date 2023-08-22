test_that("base_no_win includes all aggregates and window functions", {
  # All aggregates must be included in window functions
  expect_equal(setdiff(names(base_agg), names(base_win)), character())

  # All window functions all need to be in no_in
  expect_equal(setdiff(names(base_win), names(base_no_win)), character())
})

# mathematics --------------------------------------------------------

test_that("basic arithmetic is correct", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(1 + 2), sql("1.0 + 2.0"))
  expect_equal(test_translate_sql(2 * 4), sql("2.0 * 4.0"))
  expect_equal(test_translate_sql(5 ^ 2), sql("POWER(5.0, 2.0)"))
  expect_equal(test_translate_sql(100L %% 3L), sql("100 % 3"))

  expect_error(test_translate_sql(100L %/% 3L), "not available")
})

test_that("small numbers aren't converted to 0", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(1e-9), sql("1e-09"))
})

test_that("unary plus works with numbers", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(+10L), sql("10"))
  expect_equal(test_translate_sql(x == +10), sql('`x` = 10.0'))
  expect_equal(test_translate_sql(x %in% c(+1L, 0L)), sql('`x` IN (1, 0)'))
})

test_that("unary plus works for non-numeric expressions", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(+(1L + 2L)), sql("(1 + 2)"))
  expect_equal(test_translate_sql(mean(x, na.rm = TRUE), window = FALSE), sql('AVG(`x`)'))
})

test_that("unary minus flips sign of number", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(-10L), sql("-10"))
  expect_equal(test_translate_sql(x == -10), sql('`x` = -10.0'))
  expect_equal(test_translate_sql(x %in% c(-1L, 0L)), sql('`x` IN (-1, 0)'))
})

test_that("unary minus wraps non-numeric expressions", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(-(1L + 2L)), sql("-(1 + 2)"))
  expect_equal(test_translate_sql(-mean(x, na.rm = TRUE), window = FALSE), sql('-AVG(`x`)'))
})

test_that("binary minus subtracts", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(1L - 10L), sql("1 - 10"))
})

test_that("log base comes first", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(log(x, 10)), sql('LOG(10.0, `x`)'))
})

test_that("log becomes ln", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(log(x)), sql('LN(`x`)'))
})

test_that("can translate subsetting", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(a$b), sql("`a`.`b`"))
  expect_equal(test_translate_sql(a[["b"]]), sql("`a`.`b`"))
  expect_equal(test_translate_sql(f(a)[["b"]]), sql("f(`a`).`b`"))

  expect_equal(test_translate_sql(a[["b"]][[1]]), sql('`a`.`b`[1]'))
  expect_snapshot(error = TRUE, {
    test_translate_sql(a[[x]])
    test_translate_sql(a[[TRUE]])
  })
})


# strings -----------------------------------------------------------------

test_that("can translate case insensitive like", {
  local_con(simulate_dbi())
  test_translate_sql(str_like(x, "abc"))
  expect_snapshot(
    test_translate_sql(str_like(x, "abc", ignore_case = FALSE)),
    error = TRUE
  )
})

test_that("can translate nzchar", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(nzchar(y)), sql("((`y` IS NULL) OR `y` != '')"))
  expect_equal(test_translate_sql(nzchar(y, TRUE)), sql("`y` != ''"))
})

# aggregates --------------------------------------------------------------

test_that("all and any translated correctly", {
  db <- memdb_frame(g = c(1, 1, 2, 2, 3, 3), x = c(0, 0, 0, 1, 1, 1))

  sum_all_g <- db %>%
    group_by(g) %>%
    summarise(all = all(x == 1, na.rm = TRUE)) %>%
    filter(all) %>%
    pull(g)
  expect_equal(sum_all_g, 3)

  sum_any_g <- db %>%
    group_by(g) %>%
    summarise(any = any(x == 1, na.rm = TRUE)) %>%
    filter(any) %>%
    pull(g)
  expect_equal(sum_any_g, c(2, 3))

  win_all_g <- db %>%
    group_by(g) %>%
    filter(all(x == 1, na.rm = TRUE)) %>%
    pull(g)
  expect_equal(win_all_g, c(3, 3))

  win_any_g <- db %>%
    group_by(g) %>%
    filter(any(x == 1, na.rm = TRUE)) %>%
    pull(g)
  expect_equal(win_any_g, c(2, 2, 3, 3))
})

# binary/bitwise ---------------------------------------------------------------

test_that("bitwise operations", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(bitwNot(x)),        sql("~(`x`)"))
  expect_equal(test_translate_sql(bitwAnd(x, 128L)),  sql("`x` & 128"))
  expect_equal(test_translate_sql(bitwOr(x, 128L)),   sql("`x` | 128"))
  expect_equal(test_translate_sql(bitwXor(x, 128L)),  sql("`x` ^ 128"))
  expect_equal(test_translate_sql(bitwShiftL(x, 2L)), sql("`x` << 2"))
  expect_equal(test_translate_sql(bitwShiftR(x, 2L)), sql("`x` >> 2"))
})

test_that("default raw escapes translated correctly", {
  mf <- lazy_frame(x = "abc", con = simulate_sqlite())

  a <- blob::as_blob("abc")
  b <- blob::as_blob(as.raw(c(0x01, 0x02)))
  L <- c(a, b)

  expect_snapshot(mf %>% filter(x == a))
  expect_snapshot(mf %>% filter(x %in% L))

  qry <- mf %>% filter(x %in% !!L)
  expect_snapshot(qry)
})

# DDL ---------------------------------------------------------------------

test_that("DDL operations generate expected SQL", {
  con <- simulate_dbi()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_explain(con, sql("SELECT * FROM foo")))

  expect_snapshot(sql_query_wrap(con, ident("table")))
  expect_snapshot(sql_query_wrap(con, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_wrap(con, sql("SELECT * FROM foo")))

  expect_snapshot(sql_table_index(con, in_schema("schema", "tbl"), c("a", "b")))
  expect_snapshot(sql_table_index(con, in_schema("schema", "tbl"), "c", unique = TRUE))

  expect_snapshot(sql_query_save(con, sql("SELECT * FROM foo"), in_schema("temp", "tbl")))
})
