# handles DBI error

    Code
      (expect_error(db_analyze(con, "tbl")))
    Output
      <error/rlang_error>
      Error in `db_analyze()`:
      ! Can't analyze table tbl.
      i Using SQL: ANALYZE `tbl`
      Caused by error:
      ! dummy DBI error
    Code
      (expect_error(db_create_index(con, "tbl", "col")))
    Output
      <error/rlang_error>
      Error in `db_create_index()`:
      ! Can't create index on table tbl.
      i Using SQL: CREATE INDEX `tbl_col` ON `tbl` (`col`)
      Caused by error:
      ! dummy DBI error
    Code
      (expect_error(db_explain(con, "invalid sql")))
    Output
      <error/rlang_error>
      Error in `db_explain()`:
      ! Can't explain query.
      i Using SQL: EXPLAIN QUERY PLAN invalid sql
      Caused by error:
      ! dummy DBI error
    Code
      (expect_error(db_query_fields(con, "does not exist")))
    Output
      <error/rlang_error>
      Error in `db_query_fields()`:
      ! Can't query fields.
      i Using SQL: SELECT * FROM `does not exist` AS `q01` WHERE (0 = 1)
      Caused by error:
      ! dummy DBI error
    Code
      (expect_error(db_save_query(con, "invalid sql", "tbl")))
    Output
      <error/rlang_error>
      Error in `db_save_query()`:
      ! Can't save query to table `tbl`.
      i Using SQL: CREATE TEMPORARY TABLE `tbl` AS `invalid sql`
      Caused by error:
      ! dummy DBI error

