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

# check_from is deprecated

    Code
      tbl(con, "x", check_from = FALSE)
    Condition
      Warning:
      The `check_from` argument of `tbl_sql()` is deprecated as of dbplyr 2.5.0.
    Output
      # Source:   table<`x`> [0 x 1]
      # Database: sqlite 3.45.0 [:memory:]
      # i 1 variable: y <lgl>

