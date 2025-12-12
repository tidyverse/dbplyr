# has nice print method

    Code
      mf
    Output
      # A query:  ?? x 2
      # Database: sqlite <version> [:memory:]
            x     y
        <dbl> <dbl>
      1     1     1

---

    Code
      out2
    Output
      # A query:    ?? x 3
      # Database:   sqlite <version> [:memory:]
      # Groups:     x, y
      # Ordered by: x
            x     y     z
        <dbl> <dbl> <dbl>
      1     1     1     2

# useful error if missing I()

    Code
      tbl(src_memdb(), "foo.bar")
    Condition
      Error in `tbl()`:
      ! Failed to find table `foo.bar`.
      i Did you mean `from = I("foo.bar")`?
      Caused by error in `dbplyr_query_fields()`:
      ! Can't query fields.
      i Using SQL: SELECT * FROM `foo.bar` AS `q05` WHERE (0 = 1)
      Caused by error:
      ! no such table: foo.bar

# check_from is deprecated

    Code
      out <- tbl_sql("foo", src_dbi(con), "x", check_from = FALSE)
    Condition
      Warning:
      `tbl_sql()` was deprecated in dbplyr 2.6.0.
      Warning:
      The `check_from` argument of `tbl_sql()` is deprecated as of dbplyr 2.5.0.

