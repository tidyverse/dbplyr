test_that("with_dialect() validates inputs", {
  con <- simulate_dbi()

  expect_snapshot(error = TRUE, {
    with_dialect(NULL, dialect_postgres())
    with_dialect(con, "postgres")
  })
})

test_that("with_dialect() returns correct dialect", {
  con <- simulate_dbi()
  wrapped <- with_dialect(con, dialect_postgres())

  expect_s3_class(sql_dialect(wrapped), "sql_dialect_postgres")
})

test_that("SQL generation uses specified dialect", {
  con <- with_dialect(memdb(), dialect_postgres())
  mtcars <- local_db_table(con, mtcars, "mtcars")
  expect_snapshot(mtcars |> summarise(mpg = sd(mpg), .by = cyl) |> show_query())
})

test_that("verbs work through wrapper", {
  skip_if_not_installed("RSQLite")

  # For simple operations there's no difference between sqlite and postgres
  wrapped <- with_dialect(memdb(), dialect_postgres())

  # copy_to()
  db_mtcars <- copy_to(wrapped, mtcars, name = "mtcars")
  withr::defer(DBI::dbRemoveTable(wrapped, "mtcars"))
  expect_true(DBI::dbExistsTable(wrapped, "mtcars"))

  # compute()
  result <- db_mtcars |>
    filter(cyl == 4) |>
    select(cyl, vs, am) |>
    compute(name = "mtcars_4cyl")
  expect_true(DBI::dbExistsTable(wrapped, "mtcars_4cyl"))
  withr::defer(DBI::dbRemoveTable(wrapped, "mtcars_4cyl"))

  # collect()
  collected <- db_mtcars |>
    filter(mpg > 30) |>
    collect()
  expect_equal(nrow(collected), 4)
})
