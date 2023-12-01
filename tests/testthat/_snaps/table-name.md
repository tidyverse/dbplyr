# table_name possess key methods

    Code
      name <- table_name(c("x", "y", "z"))
      name
    Output
      <table_name> x, y, z

# can check for table name

    Code
      foo(1)
    Condition
      Error in `foo()`:
      ! `y` must be a <table_name>, not a string.
      i This is an internal error that was detected in the dbplyr package.
        Please report it at <https://github.com/tidyverse/dbplyr/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# as_table_name validates its inputs

    Code
      as_table_name("x")
    Condition
      Error in `as_table_name()`:
      ! `con` is absent but must be supplied.
    Code
      as_table_name(c("x", "y"), con)
    Condition
      Error:
      ! `c("x", "y")` must be a single string, not a character vector.
    Code
      as_table_name(1, con)
    Condition
      Error in `as_table_name()`:
      ! `1` uses unknown specification for table name
    Code
      as_table_name(I(1), con)
    Condition
      Error:
      ! `I(1)` must be a single string, not the number 1.

# as_table_name warns when using sql

    Code
      as_table_name(sql("x"), con)
    Condition
      Warning:
      `sql("x")` uses SQL where a table identifier is expected.
      i If you want to use a literal (unquoted) identifier use `I()` instead.
    Output
      <table_name> x

