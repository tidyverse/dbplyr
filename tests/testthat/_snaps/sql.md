# can format sql

    Code
      sql()
    Output
      <SQL> [empty]

---

    Code
      sql(a = "x", "y")
    Output
      <SQL> x AS a
      <SQL> y

# as.sql() is deprecated

    Code
      as.sql(ident("x"))
    Condition
      Warning:
      `as.sql()` was deprecated in dbplyr 2.6.0.
      i Please use `as_table_path()` instead.
    Output
      <IDENT> x

