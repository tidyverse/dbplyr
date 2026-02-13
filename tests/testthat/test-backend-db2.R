test_that("simulate_db2() still works", {
  expect_translation(simulate_db2(), x + 1, '"x" + 1.0')
})

test_that("uses FETCH FIRST instead of LIMIT", {
  mf <- lazy_frame(x = 1, y = 2, con = dialect_db2())
  expect_snapshot(mf |> head())
  expect_snapshot(mf |> head(10))
})

test_that("uses double quotes for identifiers", {
  con <- dialect_db2()
  expect_translation(con, first_name, '"first_name"')
})

test_that("queries with WHERE and ORDER BY work", {
  mf <- lazy_frame(x = 1, y = 2, con = dialect_db2())
  expect_snapshot(
    mf |>
      filter(x > 0) |>
      arrange(y) |>
      head(5)
  )
})
