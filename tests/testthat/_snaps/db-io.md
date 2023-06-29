# db_copy_to() wraps DBI errors

    Code
      (expect_error(db_copy_to(con = con, table = "tmp", values = data.frame(x = c(1,
        1)))))
    Output
      <error/rlang_error>
      Error in `db_copy_to()`:
      ! Can't copy to table `tmp`.
      Caused by error in `dplyr::db_write_table()`:
      ! Can't write table table `tmp`.
      Caused by error:
      ! dummy DBI error

---

    Code
      (expect_error(db_copy_to(con = con, table = "tmp2", values = data.frame(x = c(1,
        1)), unique_indexes = list("x"))))
    Output
      <error/rlang_error>
      Error in `db_copy_to()`:
      ! Can't copy to table `tmp2`.
      Caused by error in `db_create_index.DBIConnection()`:
      ! Can't create index on table `tmp2`.
      Caused by error:
      ! dummy DBI error

