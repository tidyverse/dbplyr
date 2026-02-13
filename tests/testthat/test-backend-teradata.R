test_that("simulate_teradata() still works", {
  expect_translation(simulate_teradata(), x + 1, '"x" + 1.0')
})

test_that("custom scalar translated correctly", {
  con <- dialect_teradata()

  expect_translation(con, x != y, '"x" <> "y"')
  expect_translation(
    con,
    as.numeric(x),
    'CAST("x" AS DECIMAL(12, 9))'
  )
  expect_translation(
    con,
    as.numeric(x, 8),
    'CAST("x" AS DECIMAL(12, 8))'
  )
  expect_translation(con, as.double(x), 'CAST("x" AS FLOAT)')
  expect_translation(
    con,
    as.character(x),
    'CAST("x" AS VARCHAR(255))'
  )
  expect_translation(
    con,
    as.character(x, 12),
    'CAST("x" AS VARCHAR(12))'
  )
  expect_translation(con, log(x), 'LN("x")')
  expect_translation(con, cot(x), '1 / TAN("x")')
  expect_translation(con, nchar(x), 'CHARACTER_LENGTH("x")')
  expect_translation(con, ceil(x), 'CEILING("x")')
  expect_translation(con, ceiling(x), 'CEILING("x")')
  expect_translation(con, atan2(x, y), 'ATAN2("y", "x")')
  expect_translation(
    con,
    substr(x, 1, 2),
    'SUBSTR("x", 1.0, 2.0)'
  )
  expect_translation(
    con,
    startsWith(x, "A"),
    'CAST(CASE WHEN INSTR("x", \'A\') = 1 THEN 1 ELSE 0 END AS INTEGER)'
  )
  expect_translation(con, paste0(x, y), '"x" || "y"')
  expect_translation(con, paste(x, y), '"x" || \' \' || "y"')
  expect_translation(
    con,
    as.Date("2020-01-01"),
    "DATE '2020-01-01'"
  )
  expect_translation(con, as.Date(x), 'CAST("x" AS DATE)')
  expect_translation(
    con,
    week(as.Date("2020-01-01")),
    "WEEKNUMBER_OF_YEAR(DATE '2020-01-01', 'iso')"
  )
  expect_translation(
    con,
    quarter(as.Date("2020-01-01")),
    "TO_CHAR(DATE '2020-01-01', 'q')"
  )
})

test_that("custom bitwise operations translated correctly", {
  con <- dialect_teradata()

  expect_translation(con, bitwNot(x), 'BITNOT("x")')
  expect_translation(con, bitwAnd(x, 128L), 'BITAND("x", 128)')
  expect_translation(con, bitwOr(x, 128L), 'BITOR("x", 128)')
  expect_translation(con, bitwXor(x, 128L), 'BITXOR("x", 128)')
  expect_translation(con, bitwShiftL(x, 2L), 'SHIFTLEFT("x", 2)')
  expect_translation(con, bitwShiftR(x, 2L), 'SHIFTRIGHT("x", 2)')
})

test_that("custom aggregators translated correctly", {
  con <- dialect_teradata()

  expect_translation(
    con,
    var(x, na.rm = TRUE),
    'VAR_SAMP("x")',
    window = FALSE
  )
  expect_translation(
    con,
    cumsum(x, order_by = c(A, B)),
    'SUM("x") OVER (ORDER BY "A", "B" ROWS UNBOUNDED PRECEDING)'
  )
})

test_that("custom window functions translated correctly", {
  con <- dialect_teradata()

  expect_translation(
    con,
    var(x, na.rm = TRUE),
    'VAR_SAMP("x") OVER ()'
  )
  expect_translation(
    con,
    cumsum(x, order_by = c(A, B)),
    'SUM("x") OVER (ORDER BY "A", "B" ROWS UNBOUNDED PRECEDING)'
  )
})

test_that("generates custom sql", {
  con <- dialect_teradata()

  expect_snapshot(sql_table_analyze(con, in_schema("schema", "tbl")))
})

# verb translation --------------------------------------------------------

test_that("head translated to TOP", {
  mf <- lazy_frame(x = 1, con = dialect_teradata())
  expect_snapshot(mf |> head() |> sql_render())
})

test_that("lead, lag work", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = dialect_teradata())

  expect_snapshot(
    mf |> group_by(y) |> mutate(val2 = lead(x, order_by = x)) |> sql_render()
  )
  expect_snapshot(
    mf |> group_by(y) |> mutate(val2 = lag(x, order_by = x)) |> sql_render()
  )
})


test_that("weighted.mean", {
  mf <- lazy_frame(x = c(1:5), y = c(6:10), con = dialect_teradata())

  expect_snapshot(mf |> summarise(wt_mean = weighted.mean(x, y)))
})

test_that("row_number() with and without group_by() and arrange(): unordered defaults to Ordering by NULL (per empty_order)", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = dialect_teradata())
  expect_snapshot(mf |> mutate(rown = row_number()))
  expect_snapshot(mf |> group_by(y) |> mutate(rown = row_number()))
  expect_snapshot(mf |> arrange(y) |> mutate(rown = row_number()))
})

test_that("head after distinct() produces subquery", {
  lf <- lazy_frame(x = 1, y = 2, con = dialect_teradata())
  expect_snapshot(lf |> distinct() |> head())
})
