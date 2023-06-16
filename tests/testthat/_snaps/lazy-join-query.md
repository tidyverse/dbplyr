# lazy_semi_join_query() checks arguments

    Code
      (my_lazy_semi_join_query(x = lazy_frame(x = 1)))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `x` must be a lazy query, not a <tbl_TestConnection> object.
    Code
      (my_lazy_semi_join_query(y = lazy_frame(x = 1)))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `y` must be a lazy query, not a <tbl_TestConnection> object.

---

    Code
      (my_lazy_semi_join_query(by = lmod(by0, x = 1)))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `by$x` must be a character vector, not the number 1.

---

    Code
      (my_lazy_semi_join_query(anti = NA))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `anti` must be `TRUE` or `FALSE`, not `NA`.

