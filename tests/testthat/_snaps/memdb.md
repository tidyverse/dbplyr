# src_memdb() is deprecated

    Code
      . <- src_memdb()
    Condition
      Warning:
      `src_memdb()` was deprecated in dbplyr 2.6.0.
      i Please use `memdb()` instead.

# tbl_memdb() is deprecated

    Code
      . <- tbl_memdb(data.frame(x = 1))
    Condition
      Warning:
      `tbl_memdb()` was deprecated in dbplyr 2.6.0.
      i Use `copy_to(memdb(), df)` instead

# memdb_frame() with data frame is deprecated

    Code
      . <- memdb_frame(data.frame(x = 1))
    Condition
      Warning:
      memdb_frame(data.frame(...)) was deprecated in dbplyr 2.6.0.
      i Use `copy_to(memdb(), df)` instead.

