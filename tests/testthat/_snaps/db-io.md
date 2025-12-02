# db_copy_to() wraps DBI errors

    Code
      (expect_error(db_copy_to(con = con, table = "tmp2", values = data.frame(x = c(1,
        1)), unique_indexes = list("x"))))
    Output
      <error/rlang_error>
      Error in `db_copy_to()`:
      ! Can't copy data to table `tmp2`.
      Caused by error in `db_create_index()`:
      ! Can't create index on table `tmp2`.
      i Using SQL: CREATE UNIQUE INDEX `tmp2_x` ON `tmp2` (`x`)
      Caused by error:
      ! dummy DBI error

# db_copy_to() can overwrite a table

    Code
      (expect_error(db_copy_to(con = con, table = "tmp", values = data.frame(x = c(1,
        1)))))
    Output
      <error/rlang_error>
      Error in `db_copy_to()`:
      ! Can't copy data to table `tmp`.
      Caused by error in `dplyr::db_write_table()`:
      ! Can't write table table `tmp`.
      Caused by error:
      ! dummy DBI error

# db_save_query() can overwrite a table

    Code
      (expect_error(db_save_query(con = con, sql = "SELECT 2 FROM tmp", name = "tmp"))
      )
    Output
      <error/rlang_error>
      Error in `db_save_query()`:
      ! Can't save query to table `tmp`.
      i Using SQL: CREATE TEMPORARY TABLE `tmp` AS `SELECT 2 FROM tmp`
      Caused by error:
      ! dummy DBI error

