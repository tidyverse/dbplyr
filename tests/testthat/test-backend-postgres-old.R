test_that("RPostgreSQL backend", {
  skip_if_not(identical(Sys.getenv("GITHUB_POSTGRES"), "true"))

  src <- withr::local_db_connection(
    DBI::dbConnect(
      RPostgreSQL::PostgreSQL(),
      dbname = "test",
      user = "postgres",
      password = "password",
      host = "127.0.0.1"
    )
  )

  suppressWarnings(
    copy_to(src, mtcars, "mtcars", overwrite = TRUE, temporary = FALSE)
  )
  withr::defer(DBI::dbRemoveTable(src, "mtcars"))

  expect_identical(colnames(tbl(src, "mtcars")), colnames(mtcars))

  src_cyl <- tbl(src, "mtcars") |> select(cyl) |> collect()
  expect_identical(src_cyl$cyl, mtcars$cyl)
})
