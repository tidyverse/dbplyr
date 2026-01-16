test_that("with_dialect() validates inputs", {
  con <- simulate_dbi()

  expect_snapshot(error = TRUE, {
    with_dialect(NULL, dialect_postgres())
    with_dialect(con, "postgres")
    with_dialect(con, list())
  })
})

test_that("with_dialect() returns correct dialect", {
  con <- simulate_dbi()
  wrapped <- with_dialect(con, dialect_postgres())

  expect_s3_class(sql_dialect(wrapped), "sql_dialect_postgres")
})

test_that("SQL generation uses specified dialect", {
  lf <- lazy_frame(
    x = 1,
    con = with_dialect(simulate_dbi(), dialect_postgres())
  )
  expect_snapshot(lf |> mutate(y = sd(x)))
})

test_that("DBI operations work through wrapper", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  wrapped <- with_dialect(con, dialect_postgres())
  expect_equal(sql_dialect(wrapped), dialect_postgres())

  DBI::dbWriteTable(wrapped, "test", data.frame(x = 1:3))
  expect_true(DBI::dbExistsTable(wrapped, "test"))

  result <- DBI::dbGetQuery(wrapped, "SELECT * FROM test")
  expect_equal(nrow(result), 3)
})

test_that("dplyr verbs work through wrapper", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  wrapped <- with_dialect(con, dialect_sqlite())

  # copy_to()
  db_mtcars <- copy_to(wrapped, mtcars, name = "mtcars")
  expect_true(DBI::dbExistsTable(wrapped, "mtcars"))
  expect_equal(nrow(collect(db_mtcars)), 32)

  # compute()
  result <- db_mtcars |>
    filter(cyl == 4) |>
    compute(name = "mtcars_4cyl")
  expect_true(DBI::dbExistsTable(wrapped, "mtcars_4cyl"))
  expect_equal(nrow(collect(result)), 11)

  # collect()
  collected <- db_mtcars |>
    filter(mpg > 30) |>
    collect()
  expect_equal(nrow(collected), 4)
})
