# errors for named input

    Code
      filter(lf, x = 1)
    Condition
      Error in `filter()`:
      ! Problem with `filter()` input `..1`.
      x Input `..1` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

---

    Code
      filter(lf, y > 1, x = 1)
    Condition
      Error in `filter()`:
      ! Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

# .preserve is not supported

    Code
      lf %>% filter(x == 1, .preserve = TRUE)
    Condition
      Error in `filter()`:
      ! `.preserve` is not supported on database backends

