test_that("works with RPostgreSQL backend", {
  src <- withr::local_db_connection(
    DBI::dbConnect(
      RPostgreSQL::PostgreSQL(),
      dbname = "test",
      user = "postgres",
      password = "password",
      host = "127.0.0.1"
    )
  )

  mtcars_db <- copy_to(src, mtcars, "mtcars", overwrite = TRUE)
  withr::defer(DBI::dbRemoveTable(src, "mtcars"))

  expect_identical(colnames(mtcars_db), colnames(mtcars))

  src_cyl <- tbl(src, "mtcars") |> select(cyl) |> collect()
  expect_identical(src_cyl$cyl, mtcars$cyl)
})
