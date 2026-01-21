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

# SQL generation uses specified dialect

    Code
      show_query(summarise(mtcars, mpg = sd(mpg), .by = cyl))
    Output
      <SQL>
      SELECT "cyl", STDDEV_SAMP("mpg") AS "mpg"
      FROM "mtcars"
      GROUP BY "cyl"

