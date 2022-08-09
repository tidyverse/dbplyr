test_that("db_copy_to() wraps DBI errors", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "tmp", data.frame(x = 1), overwrite = TRUE, temporary = TRUE)

  # error when writing
  expect_snapshot(
    (expect_error(
      db_copy_to(
        con = con,
        table = "tmp",
        values = data.frame(x = c(1, 1))
      )
    ))
  )

  # error when creating unique index
  expect_snapshot(
    (expect_error(
      db_copy_to(
        con = con,
        table = "tmp2",
        values = data.frame(x = c(1, 1)),
        unique_indexes = list("x")
      )
    ))
  )
})
