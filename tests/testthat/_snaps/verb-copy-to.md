# df must be a local or remote table

    Code
      copy_to(con, list(x = 1), name = "df")
    Condition
      Error in `copy_to()`:
      ! `df` must be a local dataframe or a remote tbl_sql

# as_copy() errors on invalid values

    Code
      as_copy("other")
    Condition
      Error:
      ! `copy` must be one of "none", "temp-table", or "inline", not "other".

# dbplyr_auto_copy() errors when copy = FALSE and different sources

    Code
      dbplyr_auto_copy(df, local_df, copy = FALSE)
    Condition
      Error:
      ! `x` and `y` must share the same source.
      i Use `copy = "temp-table"` to copy `y` to a temporary table.
      i USe `copy = "inline"` to convert `y` to inline SQL.

---

    Code
      dbplyr_auto_copy(df, local_df, copy = "none")
    Condition
      Error:
      ! `x` and `y` must share the same source.
      i Use `copy = "temp-table"` to copy `y` to a temporary table.
      i USe `copy = "inline"` to convert `y` to inline SQL.

