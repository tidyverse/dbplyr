test_that("base_no_win includes all aggregates and window functions", {
  # All aggregates must be included in window functions
  expect_equal(setdiff(names(base_agg), names(base_win)), character())

  # All window functions all need to be in no_in
  expect_equal(setdiff(names(base_win), names(base_no_win)), character())
})

test_that("can translate both pipes", {
  con <- simulate_dbi()
  expect_translation(
    con,
    x %>% mean() %>% sum(),
    'SUM(AVG("x"))',
    window = FALSE
  )
  expect_translation(con, x |> mean() |> sum(), 'SUM(AVG("x"))', window = FALSE)
})

# mathematics --------------------------------------------------------

test_that("basic arithmetic is correct", {
  con <- simulate_dbi()
  expect_translation(con, 1 + 2, "1.0 + 2.0")
  expect_translation(con, 2 * 4, "2.0 * 4.0")
  expect_translation(con, 5^2, "POWER(5.0, 2.0)")
  expect_translation(con, 100L %% 3L, "100 % 3")

  expect_translation_snapshot(con, 100L %/% 3L, error = TRUE)
})

test_that("small numbers aren't converted to 0", {
  con <- simulate_dbi()
  expect_translation(con, 1e-9, "1e-09")
})

test_that("unary plus works with numbers", {
  con <- simulate_dbi()
  expect_translation(con, +10L, "10")
  expect_translation(con, x == +10, '"x" = 10.0')
  expect_translation(con, x %in% c(+1L, 0L), '"x" IN (1, 0)')
})

test_that("unary plus works for non-numeric expressions", {
  con <- simulate_dbi()
  expect_translation(con, +(1L + 2L), "(1 + 2)")
  expect_translation(con, mean(x, na.rm = TRUE), 'AVG("x")', window = FALSE)
})

test_that("unary minus flips sign of number", {
  con <- simulate_dbi()
  expect_translation(con, -10L, "-10")
  expect_translation(con, -10L + x, '-10 + "x"')
  expect_translation(con, x == -10, '"x" = -10.0')
  expect_translation(con, x %in% c(-1L, 0L), '"x" IN (-1, 0)')
})

test_that("unary minus wraps non-numeric expressions", {
  con <- simulate_dbi()
  expect_translation(con, -(1L + 2L), "-(1 + 2)")
  expect_translation(con, -mean(x, na.rm = TRUE), '-AVG("x")', window = FALSE)
})

test_that("binary minus subtracts", {
  con <- simulate_dbi()
  expect_translation(con, 1L - 10L, "1 - 10")
})

test_that("log base comes first", {
  con <- simulate_dbi()
  expect_translation(con, log(x, 10), 'LOG(10.0, "x")')
})

test_that("log becomes ln", {
  con <- simulate_dbi()
  expect_translation(con, log(x), 'LN("x")')
})

test_that("can translate subsetting", {
  con <- simulate_dbi()
  expect_translation(con, a$b, '"a"."b"')
  expect_translation(con, a[["b"]], '"a"."b"')
  expect_translation(con, f(a)[["b"]], 'f("a")."b"')
  expect_translation(con, a[["b"]][[1]], '"a"."b"[1]')

  expect_translation_snapshot(con, a[[x]], error = TRUE)
  expect_translation_snapshot(con, a[[TRUE]], error = TRUE)
})

test_that("$ doesn't evaluate second argument", {
  y <- list(id = 1)

  expect_snapshot(lazy_frame(x = 1, y = 1) |> filter(x == y$id))
  expect_snapshot(lazy_frame(x = 1) |> filter(x == y$id))
})

test_that("useful error if $ used with inlined value", {
  y <- 1
  expect_snapshot(lazy_frame(x = 1) |> filter(x == y$id), error = TRUE)
})

# window ------------------------------------------------------------------

test_that("lead and lag translate n to integers", {
  con <- simulate_dbi()
  expect_translation(con, lead(x, 1), 'LEAD("x", 1, NULL) OVER ()')
  expect_translation(con, lag(x, 1), 'LAG("x", 1, NULL) OVER ()')
})

# strings -----------------------------------------------------------------

test_that("can only translate case sensitive str_like", {
  con <- simulate_dbi()
  expect_translation(con, str_like(x, "abc"), '"x" LIKE \'abc\'')

  expect_translation_snapshot(con, str_like(x, "abc", ignore_case = FALSE))
  expect_translation_snapshot(
    con,
    str_like(x, "abc", ignore_case = TRUE),
    error = TRUE
  )
  expect_translation_snapshot(
    con,
    str_ilike(x, "abc"),
    error = TRUE
  )
})

test_that("can translate nzchar", {
  con <- simulate_dbi()
  expect_translation(con, nzchar(y), '(("y" IS NULL) OR "y" != \'\')')
  expect_translation(con, nzchar(y, TRUE), '"y" != \'\'')

  # Uses individual translations from backend
  con <- simulate_access()
  expect_translation(con, nzchar(y), '(ISNULL("y") OR "y" <> \'\')')
  expect_translation(con, nzchar(y, TRUE), '"y" <> \'\'')
})

# aggregates --------------------------------------------------------------

test_that("all and any translated correctly", {
  db <- local_memdb_frame(
    "df",
    g = c(1, 1, 2, 2, 3, 3),
    x = c(0, 0, 0, 1, 1, 1)
  )

  sum_all_g <- db |>
    group_by(g) |>
    summarise(all = all(x == 1, na.rm = TRUE)) |>
    filter(all) |>
    pull(g)
  expect_equal(sum_all_g, 3)

  sum_any_g <- db |>
    group_by(g) |>
    summarise(any = any(x == 1, na.rm = TRUE)) |>
    filter(any) |>
    pull(g)
  expect_equal(sum_any_g, c(2, 3))

  win_all_g <- db |>
    group_by(g) |>
    filter(all(x == 1, na.rm = TRUE)) |>
    pull(g)
  expect_equal(win_all_g, c(3, 3))

  win_any_g <- db |>
    group_by(g) |>
    filter(any(x == 1, na.rm = TRUE)) |>
    pull(g)
  expect_equal(win_any_g, c(2, 2, 3, 3))
})

# binary/bitwise ---------------------------------------------------------------

test_that("bitwise operations", {
  con <- simulate_dbi()
  expect_translation(con, bitwNot(x), '~("x")')
  expect_translation(con, bitwAnd(x, 128L), '"x" & 128')
  expect_translation(con, bitwOr(x, 128L), '"x" | 128')
  expect_translation(con, bitwXor(x, 128L), '"x" ^ 128')
  expect_translation(con, bitwShiftL(x, 2L), '"x" << 2')
  expect_translation(con, bitwShiftR(x, 2L), '"x" >> 2')
})

test_that("default raw escapes translated correctly", {
  mf <- lazy_frame(x = "abc", con = simulate_sqlite())

  a <- blob::as_blob("abc")
  b <- blob::as_blob(as.raw(c(0x01, 0x02)))
  L <- c(a, b)

  expect_snapshot(mf |> filter(x == a))
  expect_snapshot(mf |> filter(x %in% L))

  qry <- mf |> filter(x %in% !!L)
  expect_snapshot(qry)
})
