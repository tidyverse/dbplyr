# warns informatively with unsupported function

    Code
      sql_not_supported("cor")()
    Condition
      Error:
      ! `cor()` is not available in this SQL variant.

# duplicates throw an error

    Code
      sql_translator(round = function(x) x, round = function(y) y)
    Condition
      Error in `sql_translator()`:
      ! Duplicate names in `sql_translator()`
      * round

# output of print method for sql_variant is correct

    Code
      sql_variant(sim_trans, sim_trans, sim_trans)
    Output
      <sql_variant>
      scalar:    +
      aggregate: +
      window:    +

# win_rank() is accepted by the sql_translator

    Code
      sql_variant(sql_translator(test = win_rank("test")))
    Output
      <sql_variant>
      scalar:    test

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

