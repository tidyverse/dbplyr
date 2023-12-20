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
      df %>% compute(name = in_schema("main", "db1"), temporary = FALSE)
    Condition
      Error in `db_save_query.DBIConnection()`:
      ! Can't save query to table `main`.`db1`.
      i Using SQL: CREATE TABLE `main`.`db1` AS SELECT * FROM `dbplyr_iJ60GnHlhE`
      Caused by error:
      ! table `db1` already exists

# collect() handles DBI error

    Code
      (expect_error(mf %>% mutate(a = sql("invalid sql")) %>% collect()))
    Output
      <error/rlang_error>
      Error in `collect()`:
      ! Failed to collect lazy table.
      Caused by error:
      ! dummy DBI error

# compute(temporary = FALSE) without a name is deprecated

    The `name` argument of `compute()` must be provided when `temporary = FALSE` as of dbplyr 2.3.3.

