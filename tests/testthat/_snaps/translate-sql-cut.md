# works with labels a character vector

    Code
      translate_sql(cut(x, 1:3, labels = c("a", "b", "c")), con = con)
    Condition
      Error in `cut()`:
      ! Can't recycle `labels` (size 3) to size 2.

# cut checks arguments

    Code
      translate_sql(cut(x, 1), con = con)
    Condition
      Error in `cut()`:
      ! `breaks` must have at least two values.

---

    Code
      translate_sql(cut(x, c(1, 1)), con = con)
    Condition
      Error in `cut()`:
      ! `breaks` are not unique.

---

    Code
      translate_sql(cut(x, c(1, 2, NA)), con = con)
    Condition
      Error in `cut()`:
      ! `breaks` values must not be missing.

