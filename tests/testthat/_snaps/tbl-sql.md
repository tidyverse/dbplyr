# sql tbl can be printed

    Code
      mf2
    Output
      # Source:   SQL [3 x 2]
      # Database: sqlite ?.?.? [:memory:]
            x     y
        <int> <int>
      1     1     3
      2     2     2
      3     3     1

# useful error if missing I()

    Code
      tbl(src_memdb(), "foo.bar")
    Condition
      Error in `tbl_sql()`:
      ! Failed to find table `foo.bar`.
      i Did you mean `from = I("foo.bar")`?
      Caused by error in `db_query_fields.DBIConnection()`:
      ! Can't query fields.
      i Using SQL: SELECT * FROM `foo.bar` AS `q05` WHERE (0 = 1)
      Caused by error:
      ! no such table: foo.bar

# check_from is deprecated

    Code
      tbl(con, "x", check_from = FALSE)
    Condition
      Warning:
      The `check_from` argument of `tbl_sql()` is deprecated as of dbplyr 2.5.0.
      i The deprecated feature was likely used in the dbplyr package.
        Please report the issue at <https://github.com/tidyverse/dbplyr/issues>.
    Output
      # Source:   table<`x`> [0 x 1]
      # Database: sqlite 3.45.2 [:memory:]
      # i 1 variable: y <lgl>

