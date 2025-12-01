# sql_prefix checks arguments

    Code
      sin_db(sin(1, 2))
    Condition
      Error in `sin()`:
      ! 2 arguments passed to 'sin' which requires 1

---

    Code
      sin_db(sin(a = 1))
    Condition
      Error in `sin()`:
      ! supplied argument name 'a' does not match 'x'

# runif is translated

    Code
      test_translate_sql(runif(2))
    Condition
      Error in `sql_runif()`:
      ! Only `n = n()` is supported.

