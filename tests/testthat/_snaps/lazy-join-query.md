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

