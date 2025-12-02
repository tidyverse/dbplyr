# index fails if columns are missing

    Code
      (expect_error(compute(mf, indexes = list(c("y", "x", "z"), "a"))))
    Output
      <error/rlang_error>
      Error in `compute()`:
      ! All columns specified through `indexes` must exist in `x`.
      i The following columns are missing from `indexes`: y, z, and a.
    Code
      (expect_error(compute(mf, unique_indexes = list(c("y", "x", "z"), "a"))))
    Output
      <error/rlang_error>
      Error in `compute()`:
      ! All columns specified through `unique_indexes` must exist in `x`.
      i The following columns are missing from `unique_indexes`: y, z, and a.

# compute can handle schema

    Code
      compute(df, name = in_schema("main", "db1"), temporary = FALSE)
    Condition
      Error in `db_compute()`:
      ! Can't copy query to table `main`.`db1`.
      Caused by error in `dbplyr_save_query()`:
      ! Can't save query to table `main`.`db1`.
      i Using SQL: CREATE TABLE `main`.`db1` AS SELECT * FROM `dbplyr_{tmp}`
      Caused by error:
      ! dummy DBI error

# compute(temporary = FALSE) without a name is deprecated

    The `name` argument of `compute()` must be provided when `temporary = FALSE` as of dbplyr 2.3.3.

