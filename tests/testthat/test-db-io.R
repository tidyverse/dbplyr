test_that("db_copy_to() wraps DBI errors", {
  con <- local_sqlite_connection()
  local_db_table(con, data.frame(x = 1), "tmp")

  # error when creating unique index
  expect_snapshot(
    (expect_error(
      db_copy_to(
        con = con,
        table = "tmp2",
        values = data.frame(x = c(1, 1)),
        unique_indexes = list("x")
      )
    )),
    transform = snap_transform_dbi
  )
})

test_that("db_copy_to() can overwrite a table", {
  con <- local_sqlite_connection()
  local_db_table(con, data.frame(x = 1), "tmp")

  # doesn't overwrite by default
  expect_snapshot(
    (expect_error(
      db_copy_to(
        con = con,
        table = "tmp",
        values = data.frame(x = c(1, 1))
      )
    )),
    transform = snap_transform_dbi
  )

  db_copy_to(
    con = con,
    table = "tmp",
    values = data.frame(x = c(1, 1)),
    overwrite = TRUE
  )
  expect_equal(DBI::dbReadTable(con, "tmp"), data.frame(x = c(1, 1)))
})

test_that("db_save_query() can overwrite a table", {
  con <- local_sqlite_connection()
  local_db_table(con, data.frame(x = 1), "tmp")

  # doesn't overwrite by default
  expect_snapshot(
    (expect_error(
      db_save_query(
        con = con,
        sql = "SELECT 2 FROM tmp",
        name = "tmp"
      )
    )),
    transform = snap_transform_dbi
  )

  db_save_query(
    con = con,
    sql = sql("SELECT 2 AS x"),
    name = "tmp",
    overwrite = TRUE
  )
  expect_equal(DBI::dbReadTable(con, "tmp"), data.frame(x = 2L))
})
