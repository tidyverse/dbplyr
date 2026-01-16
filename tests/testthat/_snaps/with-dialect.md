# with_dialect() validates inputs

    Code
      with_dialect(NULL, dialect_postgres())
    Condition
      Error in `with_dialect()`:
      ! `con` must not be NULL.
    Code
      with_dialect(con, "postgres")
    Condition
      Error in `with_dialect()`:
      ! `dialect` must be a dialect object.
    Code
      with_dialect(con, list())
    Condition
      Error in `with_dialect()`:
      ! `dialect` must be a dialect object.

# SQL generation uses specified dialect

    Code
      mutate(lf, y = sd(x))
    Output
      <SQL>
      SELECT *, STDDEV_SAMP("x") OVER () AS "y"
      FROM "df"

