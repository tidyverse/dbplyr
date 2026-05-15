# db_copy_to() wraps DBI errors

    Code
      (expect_error(db_copy_to(con = con, table = "tmp2", values = data.frame(x = c(1,
        1)), unique_indexes = list("x"))))
    Output
      <error/rlang_error>
      Error in `db_copy_to()`:
      ! Can't copy data to table `tmp2`.
      Caused by error in `dbplyr_create_index()`:
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
      Caused by error in `dbplyr_write_table()`:
      ! Can't write table `tmp`.
      Caused by error:
      ! dummy DBI error

