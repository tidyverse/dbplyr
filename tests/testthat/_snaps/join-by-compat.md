# join_by checks inputs

    Code
      new_join_by("x", c("x", "y"))
    Condition
      Error in `new_join_by()`:
      ! `x` and `y` must have the same length.
      i This is an internal error that was detected in the dbplyr package.
        Please report it at <https://github.com/tidyverse/dbplyr/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.
    Code
      new_join_by("x", "y", c("<", ">"))
    Condition
      Error in `new_join_by()`:
      ! Can't recycle `condition` (size 2) to size 1.

