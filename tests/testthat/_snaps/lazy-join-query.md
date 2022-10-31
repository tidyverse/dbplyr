# lazy_join_query() checks arguments

    Code
      (my_lazy_join_query(x = lazy_frame(x = 1)))
    Condition
      Error in `lazy_join_query()`:
      ! inherits(x, "lazy_query") is not TRUE
    Code
      (my_lazy_join_query(y = lazy_frame(x = 1)))
    Condition
      Error in `lazy_join_query()`:
      ! inherits(y, "lazy_query") is not TRUE

---

    Code
      (my_lazy_join_query(vars = "a"))
    Condition
      Error in `join_check_vars()`:
      ! `vars` must be a list
      i This is an internal error that was detected in the dbplyr package.
        Please report it at <https://github.com/tidyverse/dbplyr/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
    Code
      (my_lazy_join_query(vars = c(vars0, list(z = 1))))
    Condition
      Error in `join_check_vars()`:
      ! `vars` must have fields `alias`, `x`, `y`, `all_x`, and `all_y`
      i This is an internal error that was detected in the dbplyr package.
        Please report it at <https://github.com/tidyverse/dbplyr/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
    Code
      (my_lazy_join_query(vars = lmod(vars0, alias = 1)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `vars$alias` must be a vector with type <character>.
      Instead, it has type <double>.
    Code
      (my_lazy_join_query(vars = lmod(vars0, x = 1)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `vars$x` must be a vector with type <character>.
      Instead, it has type <double>.
    Code
      (my_lazy_join_query(vars = lmod(vars0, x = "a")))
    Condition
      Error in `my_lazy_join_query()`:
      ! `vars$x` must have size 3, not size 1.
    Code
      (my_lazy_join_query(vars = lmod(vars0, y = 1)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `vars$y` must be a vector with type <character>.
      Instead, it has type <double>.
    Code
      (my_lazy_join_query(vars = lmod(vars0, y = "a")))
    Condition
      Error in `my_lazy_join_query()`:
      ! `vars$y` must have size 3, not size 1.
    Code
      (my_lazy_join_query(vars = lmod(vars0, all_x = 1)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `vars$all_x` must be a vector with type <character>.
      Instead, it has type <double>.
    Code
      (my_lazy_join_query(vars = lmod(vars0, all_y = 1)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `vars$all_y` must be a vector with type <character>.
      Instead, it has type <double>.

---

    Code
      (my_lazy_join_query(by = "a"))
    Condition
      Error in `join_check_by()`:
      ! `by` must be a list
      i This is an internal error that was detected in the dbplyr package.
        Please report it at <https://github.com/tidyverse/dbplyr/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
    Code
      (my_lazy_join_query(by = lmod(by0, x = 1)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `by$x` must be a vector with type <character>.
      Instead, it has type <double>.
    Code
      (my_lazy_join_query(by = lmod(by0, y = 1)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `by$y` must be a vector with type <character>.
      Instead, it has type <double>.
    Code
      (my_lazy_join_query(by = lmod(by0, x = c("a", "b"))))
    Condition
      Error in `join_check_by()`:
      ! `by$x` and `by$y` must have the same size
      i This is an internal error that was detected in the dbplyr package.
        Please report it at <https://github.com/tidyverse/dbplyr/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.
    Code
      (my_lazy_join_query(by = lmod(by0, x_as = "a")))
    Condition
      Error in `my_lazy_join_query()`:
      ! `by$x_as` must be a vector with type <ident>.
      Instead, it has type <character>.
    Code
      (my_lazy_join_query(by = lmod(by0, x_as = ident("a", "b"))))
    Condition
      Error in `my_lazy_join_query()`:
      ! `by$x_as` must have size 1, not size 2.
    Code
      (my_lazy_join_query(by = lmod(by0, y_as = "a")))
    Condition
      Error in `my_lazy_join_query()`:
      ! `by$y_as` must be a vector with type <ident>.
      Instead, it has type <character>.
    Code
      (my_lazy_join_query(by = lmod(by0, y_as = ident("a", "b"))))
    Condition
      Error in `my_lazy_join_query()`:
      ! `by$y_as` must have size 1, not size 2.

---

    Code
      (my_lazy_join_query(type = "type"))
    Condition
      Error in `my_lazy_join_query()`:
      ! `type` must be one of "left", "right", "inner", "full", or "cross", not "type".
    Code
      (my_lazy_join_query(suffix = "_x"))
    Condition
      Error in `my_lazy_join_query()`:
      ! `suffix` must have size 2, not size 1.
    Code
      (my_lazy_join_query(suffix = c(1, 2)))
    Condition
      Error in `my_lazy_join_query()`:
      ! `suffix` must be a vector with type <character>.
      Instead, it has type <double>.
    Code
      (my_lazy_join_query(na_matches = "sometimes"))
    Condition
      Error in `my_lazy_join_query()`:
      ! `na_matches` must be one of "never" or "na", not "sometimes".

# lazy_semi_join_query() checks arguments

    Code
      (my_lazy_semi_join_query(x = lazy_frame(x = 1)))
    Condition
      Error in `lazy_semi_join_query()`:
      ! inherits(x, "lazy_query") is not TRUE
    Code
      (my_lazy_semi_join_query(y = lazy_frame(x = 1)))
    Condition
      Error in `lazy_semi_join_query()`:
      ! inherits(y, "lazy_query") is not TRUE

---

    Code
      (my_lazy_semi_join_query(by = lmod(by0, x = 1)))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `by$x` must be a vector with type <character>.
      Instead, it has type <double>.

---

    Code
      (my_lazy_semi_join_query(anti = NA))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `anti` must not be NA.
    Code
      (my_lazy_semi_join_query(na_matches = "sometimes"))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `na_matches` must be one of "never" or "na", not "sometimes".

